use crate::ilgen::IlGenerator;
use crate::symbols::{SymbolPath, SymbolTable, SymbolInfo, SymbolType};
use crate::error::{CompilerResult, wrap_option, wrap_err};
use crate::token::Location;
use crate::typing::{Type, FuncType};

use inkwell::types::{BasicMetadataTypeEnum, AnyTypeEnum};
use inkwell::values::{FunctionValue, BasicValue, BasicValueEnum, IntValue, PointerValue};
use inkwell::basic_block::BasicBlock;
use inkwell::IntPredicate;

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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>>;
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
pub struct FunctionArg {
    pub name: String,
    pub arg_type: Type,
}

#[derive(Debug)]
pub struct FunctionNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub params: Vec<FunctionArg>,
    pub scope: ScopeNode<'ctx, 'st>,
    pub ret_type: Type,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for FunctionNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()> {
        let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for param in &self.params {
            let llvm_type = wrap_option(self.location, param.arg_type.to_llvm_type(gen.context), "unable to map llvm type")?;
            match llvm_type {
                AnyTypeEnum::PointerType(pt) => args.push(pt.into()),
                AnyTypeEnum::IntType(it) => args.push(it.into()),
                AnyTypeEnum::FloatType(ft) => args.push(ft.into()),
                _ => { compiler_err!(self.location, "failed to map llvm type") },
            }
        }

        let llvm_ret_type = wrap_option(self.location, self.ret_type.to_llvm_type(gen.context), "unable to map llvm type")?;
        let fn_type = match llvm_ret_type {
            AnyTypeEnum::PointerType(pt) => pt.fn_type(&args[..], false),
            AnyTypeEnum::IntType(it) => it.fn_type(&args[..], false),
            AnyTypeEnum::FloatType(ft) => ft.fn_type(&args[..], false),
            _ => { compiler_err!(self.location, "failed to map llvm type") },
        };

        let function = gen.module.add_function(&self.name, fn_type, None);

        self.scope.generate_il(gen, &path.sub(&self.name), &function)?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {

        let mut types = Vec::<Type>::new();
        let subpath = path.sub(&self.name);
        for (i, param) in self.params.iter().enumerate() {
            types.push(param.arg_type.clone());
            symtable.add_symbol(&subpath, SymbolInfo{name: param.name.to_string(), sym_type: SymbolType::FunctionArg(i), data_type: param.arg_type.clone()});
        }
        symtable.add_symbol(path, SymbolInfo{name: self.name.to_string(), sym_type: SymbolType::Global, data_type: Type::Function(Box::new(FuncType{args: types, ret_type: self.ret_type.clone()}))});

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
        let msg = format!("unable to find function type {}", path);
        let func_symbol = wrap_option(self.location, gen.symtable.find_by_path(path), msg.as_ref())?;
        if let Type::Function(func_type) = &func_symbol.data_type {
            let value = self.expression.generate_il(gen, path, function, &func_type.ret_type)?;
            wrap_err(self.location, gen.builder.build_return(Some(value.as_ref())))?;
            return Ok(());
        }

        compiler_err!(self.location, "invalid type");
    }

    fn collect_symbols(&self, _path: &SymbolPath, _symtable: &mut SymbolTable) {}
}

#[derive(Debug)]
pub struct VarDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub expression: ExpressionBox<'ctx, 'st>,
    pub var_type: Type,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for VarDeclNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let addr = gen.alloc_var(self.location, &self.var_type, &self.name)?;
        gen.addrtable.register_ptr(path.sub(self.name.as_ref()), addr);

        let value = self.expression.generate_il(gen, path, function, &self.var_type)?;
        let int_value = basic_value_to_int(self.location, value.as_ref())?;
        wrap_err(self.location, gen.builder.build_store(addr, int_value))?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) {
        symtable.add_symbol(path, SymbolInfo{name: self.name.clone(), sym_type: SymbolType::LocalVariable, data_type: self.var_type.clone()});
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
        let cond = self.condition.generate_il(gen, path, function, &Type::Int32)?;
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


        let cond = self.condition.generate_il(gen, path, function, &Type::Int32)?;
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
        // TODO: Pass void and look up appropriate type 
        self.expression.generate_il(gen, path, function, &Type::Void)?;
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let symbol = wrap_option(self.location, gen.symtable.find_symbol(path, &self.name), "symbol not found")?;
        match symbol.sym_type {
            SymbolType::FunctionArg(index) => {
                Ok(Box::new(wrap_option(self.location, function.get_nth_param(index as u32), "invalid function argument")?.into_int_value()))
            }
            SymbolType::LocalVariable => {
                let (sym, ptr) = gen.find_symbol_with_addr(self.location, path, self.name.as_ref())?;
                if *expected_type == Type::RawPtr {
                    return Ok(Box::new(*ptr));
                }
                Ok(Box::new(gen.load_var(self.location, &sym.data_type, ptr, self.name.as_ref())?))
            }
            _ => {compiler_err!(self.location, "TODO: Implement")},
        }
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, _path: &SymbolPath, _function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let num_type = if expected_type.is_int_type() { expected_type.clone() } else { Type::Int64 };
        let llvm_type = wrap_option(self.location, num_type.to_llvm_type(gen.context), "unable to map llvm type")?;
        if let AnyTypeEnum::IntType(it) = llvm_type {
            Ok(Box::new(it.const_int(self.number, true)))
        } else {
            compiler_err!(self.location, "tried assiging number to a non int type");
        }
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, expected_type)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, expected_type)?;
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, expected_type)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, expected_type)?;
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, expected_type)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, expected_type)?;
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, expected_type)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, expected_type)?;
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, expected_type)?;
        let left_int = basic_value_to_int(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, expected_type)?;
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let left = self.left.generate_il(gen, path, function, &Type::RawPtr)?;
        let left_ptr = basic_value_to_ptr(self.location, left.as_ref())?;
        let right = self.right.generate_il(gen, path, function, expected_type)?;
        let right_int = basic_value_to_int(self.location, right.as_ref())?;

        wrap_err(self.location, gen.builder.build_store(left_ptr, right_int))?;
        Ok(Box::new(left_ptr))
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}
