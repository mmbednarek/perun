use crate::ilgen::IlGenerator;
use crate::symbols::{SymbolPath, SymbolTable, SymbolInfo, SymbolType};
use crate::error::{CompilerResult, wrap_option, wrap_err, err_with_location};
use crate::token::{Location, OperatorType};
use crate::typing::{Type, FuncType, ValueType};

use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{FunctionValue, BasicValue, BasicValueEnum, IntValue, PointerValue, BasicMetadataValueEnum};
use inkwell::basic_block::BasicBlock;
use inkwell::IntPredicate;
use either::Either;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    Assign,
    Modulo,
}

impl BinaryOperation {
    pub fn from_op_type(op_type: OperatorType) -> Option<BinaryOperation> {
        match op_type {
            OperatorType::Plus => Some(BinaryOperation::Add),
            OperatorType::Minus => Some(BinaryOperation::Subtract),
            OperatorType::Asterisk => Some(BinaryOperation::Multiply),
            OperatorType::Slash => Some(BinaryOperation::Divide),
            OperatorType::Less => Some(BinaryOperation::Less),
            OperatorType::Equals => Some(BinaryOperation::Assign),
            OperatorType::Percent => Some(BinaryOperation::Modulo),
            _ => None
        }
    }

    pub fn proceedence(&self) -> i32 {
        match self {
            BinaryOperation::Add => 3,
            BinaryOperation::Subtract => 3,
            BinaryOperation::Divide => 4,
            BinaryOperation::Multiply => 4,
            BinaryOperation::Modulo => 4,
            BinaryOperation::Less => 2,
            BinaryOperation::Assign => 1,
        }
    }

    pub fn proceeds(&self, other: &BinaryOperation) -> bool {
        self.proceedence() >= other.proceedence()
    }

    pub fn build<'ctx, 'st>(&self, gen: &mut IlGenerator<'ctx, 'st>, location: &Location, data_type: &Type, lhs: &dyn BasicValue<'ctx>, rhs: &dyn BasicValue<'ctx>) -> CompilerResult<BasicValueBox<'ctx>> {
        if *self == BinaryOperation::Assign {
            let lhs_ptr = basic_value_to_ptr(*location, lhs)?;

            if data_type.is_int_type() {
                let rhs_int = basic_value_to_int(*location, rhs)?;
                wrap_err(*location, gen.builder.build_store(lhs_ptr, rhs_int))?;
                return Ok(Box::new(lhs_ptr));
            }

            compiler_err!(*location, "undefined operation");
        }

        if data_type.is_int_type() {
            let lhs_int = basic_value_to_int(*location, lhs)?;
            let rhs_int = basic_value_to_int(*location, rhs)?;

            match self {
                BinaryOperation::Add => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_add(lhs_int, rhs_int, "sum"))?));
                },
                BinaryOperation::Subtract => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_sub(lhs_int, rhs_int, "sub"))?));
                },
                BinaryOperation::Divide => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_signed_div(lhs_int, rhs_int, "div"))?));
                },
                BinaryOperation::Multiply => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_mul(lhs_int, rhs_int, "mul"))?));
                },
                BinaryOperation::Less => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_compare(IntPredicate::SLT, lhs_int, rhs_int, "less"))?));
                },
                BinaryOperation::Modulo => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_signed_rem(lhs_int, rhs_int, "mod"))?));
                },
                _ => {
                    compiler_err!(*location, "invalid operation");
                },
            };
        }

        compiler_err!(*location, "invalid type");
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SingularOperation {
    AddressOf,
    Deference,
}

impl SingularOperation {
    pub fn from_op_type(op_type: OperatorType) -> Option<SingularOperation> {
        match op_type {
            OperatorType::Ampersand => Some(SingularOperation::AddressOf),
            OperatorType::Asterisk => Some(SingularOperation::Deference),
            _ => None
        }
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AnyOperation {
    Singular(SingularOperation),
    Binary(BinaryOperation),
    None,
}

impl AnyOperation {
    pub fn from_op_type(op_type: OperatorType, last_token_is_op: bool) -> Option<AnyOperation> {
        if last_token_is_op {
            Some(AnyOperation::Singular(SingularOperation::from_op_type(op_type)?))
        } else {
            Some(AnyOperation::Binary(BinaryOperation::from_op_type(op_type)?))
        }
    }

    pub fn proceeds(&self, other: &AnyOperation) -> bool {
        match self {
            AnyOperation::Singular(_) => true,
            AnyOperation::Binary(self_bin) => match other {
                AnyOperation::Singular(_) => false,
                AnyOperation::Binary(other_bin) => self_bin.proceeds(other_bin),
                AnyOperation::None => false,
            }
            AnyOperation::None => false,
        }
    }
}

fn basic_value_to_int<'ctx>(location: Location, value: &dyn BasicValue<'ctx>) -> CompilerResult<IntValue<'ctx>> {
    if let BasicValueEnum::IntValue(int_val) = value.as_basic_value_enum() {
        return Ok(int_val);
    } else {
        compiler_err!(location, "invalid type");
    }
}

fn basic_value_to_ptr<'ctx>(location: Location, value: &dyn BasicValue<'ctx>) -> CompilerResult<PointerValue<'ctx>> {
    if let BasicValueEnum::PointerValue(ptr_val) = value.as_basic_value_enum() {
        return Ok(ptr_val);
    } else {
        compiler_err!(location, "invalid type");
    }
}

type BasicValueBox<'ctx> = Box<dyn BasicValue<'ctx> + 'ctx>;

pub trait GlobalStatementNode<'ctx, 'st> : std::fmt::Debug {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()>;
    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()>;
}

pub trait StatementNode<'ctx, 'st> : std::fmt::Debug {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()>;
    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()>;
    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st>;
}

pub trait ExpressionNode<'ctx, 'st> : std::fmt::Debug {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>>;
    fn deduce_type(&self, symtable: &SymbolTable, path: &SymbolPath, expected_type: &Type) -> CompilerResult<Type>;
    fn get_location(&self) -> &Location;
}

#[derive(Debug)]
pub enum AnyStatementNode<'stmt, 'ctx, 'st> {
    ReturnNode(&'stmt ReturnNode<'ctx, 'st>),
    VarDeclNode(&'stmt VarDeclNode<'ctx, 'st>),
    IfNode(&'stmt IfNode<'ctx, 'st>),
    WhileNode(&'stmt WhileNode<'ctx, 'st>),
    ExpressionStatementNode(&'stmt ExpressionStatementNode<'ctx, 'st>),
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

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.collect_symbols(path, symtable)?;
        }
        Ok(())
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

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.collect_symbols(path, symtable)?;
        }
        Ok(())
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
            AnyTypeEnum::VoidType(ft) => ft.fn_type(&args[..], false),
            _ => { compiler_err!(self.location, "failed to map llvm type") },
        };

        let function = gen.module.add_function(&self.name, fn_type, None);

        gen.addrtable.register_func(path.sub(&self.name), function);

        self.scope.generate_il(gen, &path.sub(&self.name), &function)?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let mut types = Vec::<Type>::new();
        let subpath = path.sub(&self.name);
        for (i, param) in self.params.iter().enumerate() {
            types.push(param.arg_type.clone());
            symtable.add_symbol(&subpath, SymbolInfo{name: param.name.to_string(), sym_type: SymbolType::FunctionArg(i), data_type: param.arg_type.clone()});
        }
        symtable.add_symbol(path, SymbolInfo{name: self.name.to_string(), sym_type: SymbolType::Global, data_type: Type::Function(Box::new(FuncType{args: types, ret_type: self.ret_type.clone()}))});

        self.scope.collect_symbols(&subpath, symtable)
    }
}

#[derive(Debug)]
pub struct ExternFunctionNode {
    pub location: Location,
    pub name: String,
    pub params: Vec<FunctionArg>,
    pub ret_type: Type,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for ExternFunctionNode {
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
            AnyTypeEnum::VoidType(ft) => ft.fn_type(&args[..], false),
            _ => { compiler_err!(self.location, "failed to map llvm type") },
        };

        let function = gen.module.add_function(&self.name, fn_type, Some(Linkage::External));
        gen.addrtable.register_func(path.sub(&self.name), function);
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let mut types = Vec::<Type>::new();
        let subpath = path.sub(&self.name);
        for (i, param) in self.params.iter().enumerate() {
            types.push(param.arg_type.clone());
            symtable.add_symbol(&subpath, SymbolInfo{name: param.name.to_string(), sym_type: SymbolType::FunctionArg(i), data_type: param.arg_type.clone()});
        }
        symtable.add_symbol(path, SymbolInfo{name: self.name.to_string(), sym_type: SymbolType::Global, data_type: Type::Function(Box::new(FuncType{args: types, ret_type: self.ret_type.clone()}))});
        Ok(())
    }
}

// **********************************
// ********** STATEMENTS ************
// **********************************

#[derive(Debug)]
pub struct ReturnNode<'ctx, 'st> {
    pub location: Location,
    pub expression: Option<ExpressionBox<'ctx, 'st>>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for ReturnNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let msg = format!("unable to find function type {}", path);
        let func_symbol = wrap_option(self.location, gen.symtable.find_by_path(path), msg.as_ref())?;
        if let Type::Function(func_type) = &func_symbol.data_type {
            match &self.expression {
                Some(expr) => {
                    let value = expr.generate_il(gen, path, function, &func_type.ret_type, &ValueType::RValue)?;
                    wrap_err(self.location, gen.builder.build_return(Some(value.as_ref())))?;
                },
                None => {
                    wrap_err(self.location, gen.builder.build_return(None))?;
                }
            }
            return Ok(());
        }

        compiler_err!(self.location, "invalid type");
    }

    fn collect_symbols(&self, _path: &SymbolPath, _symtable: &mut SymbolTable) -> CompilerResult<()> { Ok(()) }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::ReturnNode(self)
    }
}

#[derive(Debug)]
pub struct VarDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub expression: ExpressionBox<'ctx, 'st>,
    pub var_type: Option<Type>,
}

impl<'ctx, 'st> VarDeclNode<'ctx, 'st> {
    fn deduce_type(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<Type> {
        if let Some(vt) = &self.var_type {
            Ok(vt.clone())
        } else {
            self.expression.deduce_type(&symtable, path, &Type::Void)
        }
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for VarDeclNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let symbol = err_with_location(self.location, gen.symtable.find_symbol(path, &self.name))?;
        
        let addr = gen.alloc_var(self.location, &symbol.data_type, &self.name)?;
        gen.addrtable.register_ptr(path.sub(self.name.as_ref()), addr);

        let value = self.expression.generate_il(gen, path, function, &symbol.data_type, &ValueType::RValue)?;
        if symbol.data_type.is_int_type() {
            let int_value = basic_value_to_int(self.location, value.as_ref())?;
            wrap_err(self.location, gen.builder.build_store(addr, int_value))?;
            Ok(())
        } else if symbol.data_type == Type::RawPtr {
            let ptr_value = basic_value_to_ptr(self.location, value.as_ref())?;
            wrap_err(self.location, gen.builder.build_store(addr, ptr_value))?;
            Ok(())
        } else {
            compiler_err!(self.location, "unsupported variable type")
        }

    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let var_type = self.deduce_type(path, symtable)?;
        symtable.add_symbol(path, SymbolInfo{name: self.name.clone(), sym_type: SymbolType::LocalVariable, data_type: var_type});
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::VarDeclNode(self)
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
        let cond = self.condition.generate_il(gen, path, function, &Type::Int32, &ValueType::RValue)?;
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

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        self.iftrue_scope.collect_symbols(&path.sub("if"), symtable);
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::IfNode(self)
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
        let prewhile = gen.context.append_basic_block(*function, "while.cond");
        wrap_err(self.location, gen.builder.build_unconditional_branch(prewhile))?;
        gen.builder.position_at_end(prewhile);


        let cond = self.condition.generate_il(gen, path, function, &Type::Int32, &ValueType::RValue)?;
        let cond_int = basic_value_to_int(self.location, cond.as_ref())?;

        let scope = self.scope.generate_il(gen, &path.sub("while"), function)?;
        let postwhile = gen.context.append_basic_block(*function, "while.end");

        gen.builder.position_at_end(prewhile);
        wrap_err(self.location, gen.builder.build_conditional_branch(cond_int, scope, postwhile))?;

        gen.builder.position_at_end(scope);
        wrap_err(self.location, gen.builder.build_unconditional_branch(prewhile))?;

        gen.builder.position_at_end(postwhile);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        self.scope.collect_symbols(&path.sub("while"), symtable);
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::WhileNode(self)
    }
}

#[derive(Debug)]
pub struct ExpressionStatementNode<'ctx, 'st> {
    pub expression: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for ExpressionStatementNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        self.expression.generate_il(gen, path, function, &Type::Void, &ValueType::None)?;
        Ok(())
    }

    fn collect_symbols(&self, _path: &SymbolPath, _symtable: &mut SymbolTable) -> CompilerResult<()> { Ok(()) }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::ExpressionStatementNode(self)
    }
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        let symbol = err_with_location(self.location, gen.symtable.find_symbol(path, &self.name))?;
        match symbol.sym_type {
            SymbolType::FunctionArg(index) => {
                Ok(Box::new(wrap_option(self.location, function.get_nth_param(index as u32), "invalid function argument")?))
            }
            SymbolType::LocalVariable => {
                let (sym, ptr) = gen.find_symbol_with_addr(self.location, path, self.name.as_ref())?;
                match *value_type {
                    ValueType::LValue => Ok(Box::new(*ptr)),
                    ValueType::RValue => Ok(Box::new(gen.load_var(self.location, &sym.data_type, ptr, self.name.as_ref())?)),
                    ValueType::None => Ok(gen.null_ptr()),
                }
            }
            _ => {compiler_err!(self.location, "TODO: Implement")},
        }
    }

    fn deduce_type(&self, symtable: &SymbolTable, path: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        let symbol = err_with_location(self.location, symtable.find_symbol(path, &self.name))?;
        Ok(symbol.data_type.clone())
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
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, _function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret a number as an l-value");
        }

        let num_type = self.deduce_type(&gen.symtable, path, expected_type)?;
        let llvm_type = wrap_option(self.location, num_type.to_llvm_type(gen.context), "unable to map llvm type")?;
        if let AnyTypeEnum::IntType(it) = llvm_type {
            Ok(Box::new(it.const_int(self.number, true)))
        } else {
            compiler_err!(self.location, "tried assiging number to a non int type");
        }
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        if expected_type.is_int_type() {
            Ok(expected_type.clone())
        } else {
            if u32::try_from(self.number).is_ok() {
                Ok(Type::Int32)
            } else {
                Ok(Type::Int64)
            }
        }
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct StringNode {
    pub location: Location,
    pub value: String,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for StringNode {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, _: &SymbolPath, _function: &FunctionValue<'ctx>, _: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret a string as an l-value");
        }

        let arr = gen.context.i8_type().array_type(self.value.len() as u32);
        let global_val = gen.module.add_global(arr, None, "str");
        global_val.set_constant(true);

        let str_val = gen.context.const_string(self.value.as_bytes(), true);
        global_val.set_initializer(&str_val);

        Ok(Box::new(global_val.as_pointer_value()))
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(Type::RawPtr)
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct BinaryExpressionNode<'ctx, 'st> {
    pub location: Location,
    pub operation: BinaryOperation,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for BinaryExpressionNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret assignment expression as an l-value");
        }

        // For assigment we always expect the left hand side to be an l-value.

        let left_deduced_type = self.left.deduce_type(gen.symtable, path, expected_type)?;
        let right_deduced_type = self.right.deduce_type(gen.symtable, path, expected_type)?;

        let (left_value_type, deduced_type) = if self.operation == BinaryOperation::Assign {
            if left_deduced_type.is_void() {
                (&ValueType::LValue, right_deduced_type)
            } else {
                (&ValueType::LValue, left_deduced_type)
            }
        } else {
            (&ValueType::RValue, left_deduced_type.wider_type(&right_deduced_type))
        };

        let left = self.left.generate_il(gen, path, function, &deduced_type, left_value_type)?;
        let right = self.right.generate_il(gen, path, function, &deduced_type, &ValueType::RValue)?;


        self.operation.build(gen, &self.location, &deduced_type, left.as_ref(), right.as_ref())
    }

    fn deduce_type(&self, symtable: &SymbolTable, path: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        self.left.deduce_type(symtable, path, expected_type)
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct SingularExpressionNode<'ctx, 'st> {
    pub location: Location,
    pub operation: SingularOperation,
    pub expr: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for SingularExpressionNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        match self.operation {
            SingularOperation::AddressOf => {
                if *value_type == ValueType::LValue {
                    compiler_err!(self.location, "tried to interpret assignment expression as an l-value");
                }
                self.expr.generate_il(r#gen, path, function, expected_type, &ValueType::LValue)
            },
            SingularOperation::Deference => {
                let ptr_box = self.expr.generate_il(r#gen, path, function, &Type::RawPtr, &ValueType::RValue)?;
                match value_type {
                    ValueType::LValue => {
                        Ok(ptr_box)
                    },
                    ValueType::RValue => {
                        let ptr = basic_value_to_ptr(self.location, ptr_box.as_ref())?;
                        Ok(Box::new(gen.load_var(self.location, expected_type, &ptr, "deref")?))
                    },
                    ValueType::None => {
                        Ok(gen.null_ptr())
                    }
                }
            },
        }
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        match self.operation {
            SingularOperation::AddressOf => {
                Ok(Type::RawPtr)
            },
            SingularOperation::Deference => {
                Ok(expected_type.clone())
            },
        }
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct FunctionCall<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub args: Vec<ExpressionBox<'ctx, 'st>>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for FunctionCall<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, _expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret function call as an l-value");
        }

        let symbol = err_with_location(self.location, gen.symtable.find_symbol(path, &self.name))?;
        if let Type::Function(fn_type) = &symbol.data_type {
            if fn_type.args.len() != self.args.len() {
                compiler_err!(self.location, "invalid number of arguments");
            }

            let mut call_args = Vec::<BasicMetadataValueEnum>::new();
            for (i, arg_expr) in self.args.iter().enumerate() {
                let arg_type = &fn_type.args[i];
                let arg_value = arg_expr.generate_il(gen, path, function, arg_type, &ValueType::RValue)?;
                let arg_value_enum = arg_value.as_basic_value_enum();
                call_args.push(arg_value_enum.into());
            }

            let func = wrap_option(self.location, gen.addrtable.find_func(path, &self.name), "no function found.")?;
            let call_res = wrap_err(self.location, gen.builder.build_call(*func, call_args.as_ref(), self.name.as_ref()))?;
            let res = call_res.try_as_basic_value();

            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(gen.null_ptr())
            }
        } else {
            compiler_err!(self.location, "tried to call an object that's not a function");
        }
    }

    fn deduce_type(&self, symtable: &SymbolTable, path: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        let symbol = err_with_location(self.location, symtable.find_symbol(path, &self.name))?;
        if let Type::Function(fn_type) = &symbol.data_type {
            return Ok(fn_type.ret_type.clone());
        }

        compiler_err!(self.location, "tried to call an object that's not a function");
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct GetElementNode<'ctx, 'st> {
    pub location: Location,
    pub object: ExpressionBox<'ctx, 'st>,
    pub index: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for GetElementNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        let obj_value = self.object.generate_il(gen, path, function, &Type::RawPtr, &ValueType::RValue)?;
        let obj_ptr = basic_value_to_ptr(self.location, obj_value.as_ref())?;
        let index_value = self.index.generate_il(gen, path, function, &Type::Int32, &ValueType::RValue)?;
        let index = basic_value_to_int(self.location, index_value.as_ref())?;

        let indexed_ptr = gen.build_get_element_ptr(self.location, expected_type, obj_ptr, index, "addrindex")?;
        match value_type {
            ValueType::LValue => Ok(Box::new(indexed_ptr)),
            ValueType::RValue => Ok(Box::new(gen.load_var(self.location, expected_type, &indexed_ptr, "addrvalue")?)),
            ValueType::None => Ok(gen.null_ptr()),
        }
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        Ok(expected_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct CastNode<'ctx, 'st> {
    pub location: Location,
    pub target_type: Type,
    pub expr: ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for CastNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "expression is not R-value");
        }

        let source_type = self.expr.deduce_type(&gen.symtable, path, expected_type)?;
        if !self.target_type.is_int_type() || !source_type.is_int_type() {
            compiler_err!(self.location, "only int cast is supported");
        }

        let expr = self.expr.generate_il(gen, path, function, &source_type, &ValueType::RValue)?;
        let int_value = basic_value_to_int(self.location, expr.as_ref())?;

        if self.target_type.byte_count() > source_type.byte_count() {
            Ok(Box::new(gen.build_sext(self.location, &self.target_type, int_value, "intsext")?))
        } else {
            Ok(Box::new(gen.build_trunc(self.location, &self.target_type, int_value, "inttrunc")?))
        }
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(self.target_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}