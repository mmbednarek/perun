use crate::ilgen::IlGenerator;
use crate::symbols::{SymbolPath, SymbolTable, SymbolInfo, SymbolType};
use crate::error::{CompilerResult, CompilerError};
use crate::token::Location;

use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{FunctionValue, BasicValue, BasicValueEnum, IntValue, PointerValue};
use inkwell::basic_block::BasicBlock;
use inkwell::IntPredicate;
use inkwell::builder::BuilderError;

fn basic_value_to_int<'ctx>(location: Location, value: &dyn BasicValue<'ctx>) -> CompilerResult<IntValue<'ctx>> {
    if let BasicValueEnum::IntValue(int_val) = value.as_basic_value_enum() {
        return Ok(int_val);
    }
    compiler_err!(location, "invalid type")
}

fn basic_value_to_ptr<'ctx>(location: Location, value: &dyn BasicValue<'ctx>) -> CompilerResult<PointerValue<'ctx>> {
    if let BasicValueEnum::PointerValue(ptr_val) = value.as_basic_value_enum() {
        return Ok(ptr_val);
    }
    compiler_err!(location, "invalid type")
}

fn wrap_err<T>(loc: Location, res: Result<T, BuilderError>)  -> CompilerResult<T> {
    res.map_err(|be| CompilerError{message: format!("builder error: {:?}", be), location: loc})
}

fn wrap_option<T>(loc: Location, res: Option<T>, msg: &str)  -> CompilerResult<T> {
    match res {
        Some(value) => Ok(value),
        None => compiler_err!(loc, "{}", msg),
    }
}

type BasicValueBox<'ctx> = Box<dyn BasicValue<'ctx> + 'ctx>;

pub trait GlobalStatementNode<'ctx, 'st> : std::fmt::Debug {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()>;
    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable);
}

pub trait StatementNode<'ctx, 'st> : std::fmt::Debug {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()>;
    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable);
}

pub trait ExpressionNode<'ctx, 'st> : std::fmt::Debug {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>>;
    fn get_location(&self) -> &Location;
}

type ExpressionBox<'ctx, 'st> = Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>;

// **********************************
// ******** GLOBAL STATEMENTS *******
// **********************************

#[derive(Debug)]
pub struct SourceUnit<'ctx, 'st> {
    pub body: Vec<Box<dyn GlobalStatementNode<'ctx, 'st> + 'ctx>>,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for SourceUnit<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.generate_il(gen, path)?;
        }
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        for stmt in &self.body {
            stmt.collect_symbols(path, symtable);
        }
    }
}

#[derive(Debug)]
pub struct ScopeNode<'ctx, 'st> {
    pub body: Vec<Box<dyn StatementNode<'ctx, 'st> + 'ctx>>,
    pub name: String,
}

impl<'ctx, 'st> ScopeNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<BasicBlock<'ctx>> {
        let basic_block = gen.context.append_basic_block(*function, &self.name);
        gen.builder.position_at_end(basic_block);

        for stmt in &self.body {
            stmt.generate_il(gen, path, function)?;
        }
        
        Ok(basic_block)
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        for stmt in &self.body {
            stmt.collect_symbols(path, symtable);
        }
    }
}

#[derive(Debug)]
pub struct FunctionNode<'ctx, 'st> {
    pub name: String,
    pub params: Vec<String>,
    pub scope: ScopeNode<'ctx, 'st>,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for FunctionNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()> {
        let i64_type = gen.context.i64_type();
        let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for _ in &self.params {
            args.push(i64_type.into());
        }
        let fn_type = i64_type.fn_type(&args[..], false);

        let function = gen.module.add_function(&self.name, fn_type, None);

        self.scope.generate_il(gen, &path.sub(&self.name), &function)?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        let subpath = path.sub(&self.name);
        for (i, param) in self.params.iter().enumerate() {
            symtable.add_symbol(&subpath, SymbolInfo{name: param.to_string(), sym_type: SymbolType::FunctionArg(i)});
        }
        self.scope.collect_symbols(&subpath, symtable);
    }
}

// **********************************
// ********** STATEMENTS ************
// **********************************

#[derive(Debug)]
pub struct ReturnNode<'ctx, 'st> {
    pub location: Location,
    pub expression: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for ReturnNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let value = self.expression.generate_il(gen, path, function, false)?;
        wrap_err(self.location, gen.builder.build_return(Some(value.as_ref())))?;
        Ok(())
    }

    fn collect_symbols(&self, _path: &SymbolPath, _symtable: &mut SymbolTable) {}
}

#[derive(Debug)]
pub struct VarDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub expression: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for VarDeclNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let addr = wrap_err(self.location, gen.builder.build_alloca(gen.context.i64_type(), &self.name))?;
        gen.addrtable.register_ptr(path.sub(self.name.as_ref()), addr);

        let value = self.expression.generate_il(gen, path, function, false)?;
        let int_value = basic_value_to_int(self.location, value.as_ref())?;
        wrap_err(self.location, gen.builder.build_store(addr, int_value))?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        symtable.add_symbol(path, SymbolInfo{name: self.name.clone(), sym_type: SymbolType::LocalVariable});
    }
}

#[derive(Debug)]
pub struct IfNode<'ctx, 'st> {
    pub location: Location,
    pub condition: ExpressionBox<'ctx, 'st>,
    pub iftrue_scope: ScopeNode<'ctx, 'st>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for IfNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let cond = self.condition.generate_il(gen, path, function, false)?;
        let cond_int = basic_value_to_int(self.location, cond.as_ref())?;

        let current_block = wrap_option(self.location, gen.builder.get_insert_block(), "statement not located in a valid block")?;

        let iftrue = self.iftrue_scope.generate_il(gen, &path.sub("if"), function)?;
        let postif = gen.context.append_basic_block(*function, "postif");

        gen.builder.position_at_end(current_block);
        wrap_err(self.location, gen.builder.build_conditional_branch(cond_int, iftrue, postif))?;

        gen.builder.position_at_end(iftrue);
        wrap_err(self.location, gen.builder.build_unconditional_branch(postif))?;

        gen.builder.position_at_end(postif);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        self.iftrue_scope.collect_symbols(&path.sub("if"), symtable);
    }
}

#[derive(Debug)]
pub struct WhileNode<'ctx, 'st> {
    pub location: Location,
    pub condition: ExpressionBox<'ctx, 'st>,
    pub scope: ScopeNode<'ctx, 'st>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for WhileNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()>{
        let prewhile = gen.context.append_basic_block(*function, "prewhile");
        wrap_err(self.location, gen.builder.build_unconditional_branch(prewhile))?;
        gen.builder.position_at_end(prewhile);


        let cond = self.condition.generate_il(gen, path, function, false)?;
        let cond_int = basic_value_to_int(self.location, cond.as_ref())?;

        let scope = self.scope.generate_il(gen, &path.sub("while"), function)?;
        let postwhile = gen.context.append_basic_block(*function, "postwhile");

        gen.builder.position_at_end(prewhile);
        wrap_err(self.location, gen.builder.build_conditional_branch(cond_int, scope, postwhile))?;

        gen.builder.position_at_end(scope);
        wrap_err(self.location, gen.builder.build_unconditional_branch(prewhile))?;

        gen.builder.position_at_end(postwhile);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        self.scope.collect_symbols(&path.sub("while"), symtable);
    }
}

#[derive(Debug)]
pub struct ExpressionStatementNode<'ctx, 'st> {
    pub expression: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for ExpressionStatementNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        self.expression.generate_il(gen, path, function, false)?;
        Ok(())
    }

    fn collect_symbols(&self, _path: &SymbolPath, _symtable: &mut SymbolTable) {}
}

// **********************************
// ********** EXPRESSIONS ***********
// **********************************

#[derive(Debug)]
pub struct IdentifierNode {
    pub location: Location,
    pub name: String,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for IdentifierNode {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let symbol = wrap_option(self.location, gen.symtable.find_symbol(path, &self.name), "symbol not found")?;
        match symbol.sym_type {
            SymbolType::FunctionArg(index) => {
                return Ok(Box::new(wrap_option(self.location, function.get_nth_param(index as u32), "invalid function argument")?.into_int_value()));
            }
            SymbolType::LocalVariable => {
                let ptr = wrap_option(self.location, gen.addrtable.find_symbol(path, self.name.as_ref()), "unable to find address for the symbol")?;
                if expects_ptr {
                    return Ok(Box::new(*ptr));
                }
                return Ok(Box::new(wrap_err(self.location, gen.builder.build_load(gen.context.i64_type(), *ptr, self.name.as_ref()))?));
            }
        };
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct NumberNode {
    pub location: Location,
    pub number: u64,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for NumberNode {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, _path: &SymbolPath, _function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        Ok(Box::new(gen.context.i64_type().const_int(self.number, true)))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct PlusNode<'ctx, 'st> {
    pub location: Location,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for PlusNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, false)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, false)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        Ok(Box::new(wrap_err(self.location, gen.builder.build_int_add(left_int, right_int, "sum"))?))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct MinusNode<'ctx, 'st> {
    pub location: Location,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for MinusNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, false)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, false)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        Ok(Box::new(wrap_err(self.location, gen.builder.build_int_sub(left_int, right_int, "sub"))?))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct MultNode<'ctx, 'st> {
    pub location: Location,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for MultNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, false)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, false)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        Ok(Box::new(wrap_err(self.location, gen.builder.build_int_mul(left_int, right_int, "mult"))?))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct DivNode<'ctx, 'st> {
    pub location: Location,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for DivNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, false)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, false)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        Ok(Box::new(wrap_err(self.location, gen.builder.build_int_signed_div(left_int, right_int, "div"))?))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct LessNode<'ctx, 'st> {
    pub location: Location,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for LessNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, false)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, false)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        Ok(Box::new(wrap_err(self.location, gen.builder.build_int_compare(IntPredicate::SLT, left_int, right_int, "less"))?))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct AssignNode<'ctx, 'st> {
    pub location: Location,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for AssignNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expects_ptr: bool) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, true)?;
        let left_ptr = basic_value_to_ptr(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, false)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        wrap_err(self.location, gen.builder.build_store(left_ptr, right_int))?;
        Ok(Box::new(left_ptr))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}
