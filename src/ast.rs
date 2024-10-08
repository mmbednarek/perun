use crate::ilgen::{IlGenerator, basic_value_to_int, basic_value_to_ptr};
use crate::symbols::{SymbolPath, SymbolTable, SymbolInfo, SymbolType};
use crate::error::{CompilerResult, wrap_option, wrap_err, err_with_location};
use crate::token::{Location, OperatorType};
use crate::typing::{FuncType, FuncTypeArg, StructType, Type, ValueType};
use crate::{visit_type, visit_any_type};

use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType};
use inkwell::values::{FunctionValue, BasicValue, BasicMetadataValueEnum};
use inkwell::basic_block::BasicBlock;
use inkwell::{AddressSpace, IntPredicate};
use either::Either;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equals,
    NotEquals,
    Assign,
    Modulo,
    LogicalAnd,
    LogicalOr,
}

impl BinaryOperation {
    pub fn from_op_type(op_type: OperatorType) -> Option<BinaryOperation> {
        match op_type {
            OperatorType::Plus => Some(BinaryOperation::Add),
            OperatorType::Minus => Some(BinaryOperation::Subtract),
            OperatorType::Asterisk => Some(BinaryOperation::Multiply),
            OperatorType::Slash => Some(BinaryOperation::Divide),
            OperatorType::Less => Some(BinaryOperation::Less),
            OperatorType::LessOrEqual => Some(BinaryOperation::LessOrEqual),
            OperatorType::Greater => Some(BinaryOperation::Greater),
            OperatorType::GreaterOrEqual => Some(BinaryOperation::GreaterOrEqual),
            OperatorType::EqualsEquals => Some(BinaryOperation::Equals),
            OperatorType::NotEquals => Some(BinaryOperation::NotEquals),
            OperatorType::Equals => Some(BinaryOperation::Assign),
            OperatorType::Percent => Some(BinaryOperation::Modulo),
            OperatorType::LogicalAnd => Some(BinaryOperation::LogicalAnd),
            OperatorType::LogicalOr => Some(BinaryOperation::LogicalOr),
            _ => None
        }
    }

    pub fn is_predicate(&self) -> bool {
        match self {
            BinaryOperation::Less => true,
            BinaryOperation::LessOrEqual => true,
            BinaryOperation::Greater => true,
            BinaryOperation::GreaterOrEqual => true,
            BinaryOperation::Equals => true,
            BinaryOperation::NotEquals => true,
            BinaryOperation::LogicalAnd => true,
            BinaryOperation::LogicalOr => true,
            _ => false,
        }
    }

    pub fn is_logical_predicate(&self) -> bool {
        match self {
            BinaryOperation::LogicalAnd => true,
            BinaryOperation::LogicalOr => true,
            _ => false,
        }
    }

    pub fn proceedence(&self) -> i32 {
        match self {
            BinaryOperation::Add => 5,
            BinaryOperation::Subtract => 5,
            BinaryOperation::Divide => 6,
            BinaryOperation::Multiply => 6,
            BinaryOperation::Modulo => 6,
            BinaryOperation::Less => 4,
            BinaryOperation::LessOrEqual => 4,
            BinaryOperation::Greater => 4,
            BinaryOperation::GreaterOrEqual => 4,
            BinaryOperation::Equals => 4,
            BinaryOperation::NotEquals => 4,
            BinaryOperation::LogicalAnd => 3,
            BinaryOperation::LogicalOr => 2,
            BinaryOperation::Assign => 1,
        }
    }

    pub fn to_llvm_int_predicate(&self) -> Option<IntPredicate> {
        match self {
            BinaryOperation::Less => Some(IntPredicate::SLT),
            BinaryOperation::LessOrEqual => Some(IntPredicate::SLE),
            BinaryOperation::Greater => Some(IntPredicate::SGT),
            BinaryOperation::GreaterOrEqual => Some(IntPredicate::SGE),
            BinaryOperation::Equals => Some(IntPredicate::EQ),
            BinaryOperation::NotEquals => Some(IntPredicate::NE),
            _ => None,
        }
    }

    pub fn proceeds(&self, other: &BinaryOperation) -> bool {
        self.proceedence() >= other.proceedence()
    }

    pub fn build<'ctx, 'st>(&self, gen: &mut IlGenerator<'ctx, 'st>, location: &Location, operand_type: &Type, lhs: &dyn BasicValue<'ctx>, rhs: &dyn BasicValue<'ctx>) -> CompilerResult<BasicValueBox<'ctx>> {
        if *self == BinaryOperation::Assign {
            let lhs_ptr = basic_value_to_ptr(*location, lhs)?;

            if operand_type.is_int_type() {
                let rhs_int = basic_value_to_int(*location, rhs)?;
                wrap_err(*location, gen.builder.build_store(lhs_ptr, rhs_int))?;
                return Ok(Box::new(lhs_ptr));
            } else if operand_type.is_ptr_type() {
                let rhs_ptr = basic_value_to_ptr(*location, rhs)?;
                wrap_err(*location, gen.builder.build_store(lhs_ptr, rhs_ptr))?;
                return Ok(Box::new(lhs_ptr));
            }

            compiler_err!(*location, "undefined operation");
        }

        if operand_type.is_int_type() {
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
                BinaryOperation::Modulo => {
                    return Ok(Box::new(wrap_err(*location, gen.builder.build_int_signed_rem(lhs_int, rhs_int, "mod"))?));
                },
                binary_op => {
                    if let Some(predicate) = binary_op.to_llvm_int_predicate() {
                        return Ok(Box::new(wrap_err(*location, gen.builder.build_int_compare(predicate, lhs_int, rhs_int, "pred"))?));
                    } else {
                        compiler_err!(*location, "invalid operation");
                    }
                },
            };
        } else if operand_type.is_ptr_type() {
            let lhs_ptr = basic_value_to_ptr(*location, lhs)?;
            let rhs_ptr = basic_value_to_ptr(*location, rhs)?;

            if let Some(predicate) = self.to_llvm_int_predicate() {
                return Ok(Box::new(wrap_err(*location, gen.builder.build_int_compare(predicate, lhs_ptr, rhs_ptr, "pred"))?));
            } else {
                compiler_err!(*location, "invalid operation");
            }
        }

        compiler_err!(*location, "invalid type");
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SingularOperation {
    AddressOf,
    Deference,
    Not,
    Minus,
}

impl SingularOperation {
    pub fn from_op_type(op_type: OperatorType) -> Option<SingularOperation> {
        match op_type {
            OperatorType::Ampersand => Some(SingularOperation::AddressOf),
            OperatorType::Asterisk => Some(SingularOperation::Deference),
            OperatorType::Not => Some(SingularOperation::Not),
            OperatorType::Minus => Some(SingularOperation::Minus),
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

    fn build_boolean_branch(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, true_block: BasicBlock<'ctx>, false_block: BasicBlock<'ctx>) -> CompilerResult<()> {
        let value = self.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
        let value_int = basic_value_to_int(*self.get_location(), value.as_ref())?;
        wrap_err(*self.get_location(), gen.builder.build_conditional_branch(value_int, true_block, false_block))?;
        Ok(())
    }

    fn generate_boolean(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, true_block: BasicBlock<'ctx>, false_block: BasicBlock<'ctx>) -> CompilerResult<()> {
        self.build_boolean_branch(gen, path, function, true_block, false_block)
    }

    fn generate_casted(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        let stmt = self.generate_il(gen, path, function, expected_type, value_type)?;
        if *value_type == ValueType::LValue {
            Ok(stmt)
        } else {
            let deduced_type = self.deduce_type(&gen.symtable, path, expected_type)?;
            gen.build_cast(*self.get_location(), &deduced_type, expected_type, stmt)
        }
    }

    fn to_constexpr_value(&self, _: &mut IlGenerator<'ctx, 'st>, _: &SymbolPath, _: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        compiler_err!(*self.get_location(), "expression cannot be resolved at compile time");
    }
}

#[derive(Debug)]
pub enum AnyStatementNode<'stmt, 'ctx, 'st> {
    ReturnNode(&'stmt ReturnNode<'ctx, 'st>),
    VarDeclNode(&'stmt VarDeclNode<'ctx, 'st>),
    RefDeclNode(&'stmt RefDeclNode<'ctx, 'st>),
    IfNode(&'stmt IfNode<'ctx, 'st>),
    WhileNode(&'stmt WhileNode<'ctx, 'st>),
    ExpressionStatementNode(&'stmt ExpressionStatementNode<'ctx, 'st>),
}

pub type ExpressionBox<'ctx, 'st> = Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>;

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

        self.generate_to_current_block(gen, path, function)?;
        
        Ok(basic_block)
    }

    fn generate_to_current_block(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.generate_il(gen, path, function)?;
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
pub struct ConstDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub const_type: Option<Type>,
    pub value : ExpressionBox<'ctx, 'st>,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for ConstDeclNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()> {
        let sym_path = path.sub(&self.name);
        let sym = wrap_option(self.location, gen.symtable.find_by_path(&sym_path), "internal error")?;
        let basic_val = self.value.to_constexpr_value(gen, path, &sym.data_type)?;
        gen.addrtable.register_basic_value(sym_path, basic_val);
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let expected_type = match self.const_type.clone() {
            Some(t) => t,
            None => self.value.deduce_type(symtable, path, &Type::Void)?,
        };
        err_with_location(self.location, symtable.add_symbol(path, SymbolInfo{name: self.name.clone(), sym_type: SymbolType::ConstantDef, data_type: expected_type, location: self.location}))?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionArg {
    pub location: Location,
    pub name: String,
    pub arg_type: Type,
    pub is_ref: bool,
}

#[derive(Debug)]
pub enum FunctionLinkage {
    Standard,
    External,
}

#[derive(Debug)]
pub struct FunctionNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub params: Vec<FunctionArg>,
    pub ret_type: Type,
    pub linkage: FunctionLinkage,
    pub scope: Option<ScopeNode<'ctx, 'st>>,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for FunctionNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath) -> CompilerResult<()> {
        let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for param in &self.params {
            if param.is_ref {
                args.push(gen.context.ptr_type(AddressSpace::from(0)).into());
            } else {
                visit_type!(param.location, gen.context, &param.arg_type, value, Ok(args.push(value.into())))?;
            }
        }

        let fn_type = visit_any_type!(self.location, gen.context, &self.ret_type, value, Ok(value.fn_type(&args[..], false)))?;

        let linkage = match self.linkage {
            FunctionLinkage::Standard => None,
            FunctionLinkage::External => Some(Linkage::External),
        };

        let function = gen.module.add_function(&self.name, fn_type, linkage);
        gen.addrtable.register_func(path.sub(&self.name), function);

        match &self.scope {
            Some(scope) => {
                let basic_block = gen.context.append_basic_block(function, &self.name);
                gen.builder.position_at_end(basic_block);

                let func_path = path.sub(&self.name);
                for (path, sym) in gen.symtable.iterate_path(&func_path) {
                    match sym.sym_type {
                        SymbolType::LocalVariable => {
                            let resolved_type = err_with_location(self.location, gen.symtable.resolve_type_alias(path, sym.data_type.clone()))?;
                            let addr = gen.alloc_var(sym.location, &resolved_type, &sym.name)?;
                            gen.addrtable.register_ptr(path.clone(), addr);
                        },
                        SymbolType::LocalReference => {
                            let addr = gen.alloc_var(sym.location, &Type::RawPtr, &sym.name)?;
                            gen.addrtable.register_ptr(path.clone(), addr);
                        },
                        _ => {},
                    }
                }

                scope.generate_to_current_block(gen, &func_path, &function)?;
            },
            None => {},
        }

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let mut types = Vec::<FuncTypeArg<Type>>::new();
        let subpath = path.sub(&self.name);
        for (i, param) in self.params.iter().enumerate() {
            types.push(FuncTypeArg{is_ref: param.is_ref, arg_type: param.arg_type.clone()});
            err_with_location(self.location, symtable.add_symbol(&subpath, SymbolInfo{name: param.name.to_string(), sym_type: SymbolType::FunctionArg(i, param.is_ref), data_type: param.arg_type.clone(), location: self.location}))?;
        }
        err_with_location(self.location, symtable.add_symbol(path, SymbolInfo{name: self.name.to_string(), sym_type: SymbolType::FunctionDef, data_type: Type::Function(Box::new(FuncType{args: types, ret_type: self.ret_type.clone()})), location: self.location}))?;

        match &self.scope {
            Some(scope) => {scope.collect_symbols(&subpath, symtable)?; },
            None => {},
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct StructField {
    pub location: Location,
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug)]
pub struct StructNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<StructField>,
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for StructNode {
    fn generate_il(&self, _: &mut IlGenerator<'ctx, 'st>, _: &SymbolPath) -> CompilerResult<()> {
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        for (i, field) in self.fields.iter().enumerate() {
            let subpath = path.sub(&self.name);
            err_with_location(self.location, symtable.add_symbol(&subpath, SymbolInfo{name: field.name.clone(), sym_type: SymbolType::StructField(i), data_type: field.field_type.clone(), location: field.location}))?;
        }

        let fields: Vec<Type> = self.fields.iter().map(|field| field.field_type.clone()).collect();
        let struct_type = StructType{fields};
        let symbol = SymbolInfo{name: self.name.clone(), sym_type: SymbolType::TypeDef, data_type: Type::Struct(Box::new(struct_type)), location: self.location};
        err_with_location(self.location, symtable.add_symbol(path, symbol))?;
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
                    let value = expr.generate_casted(gen, path, function, &func_type.ret_type, &ValueType::RValue)?;
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
    pub expression: Option<ExpressionBox<'ctx, 'st>>,
    pub var_type: Option<Type>,
}

impl<'ctx, 'st> VarDeclNode<'ctx, 'st> {
    fn deduce_type(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<Type> {
        if let Some(vt) = &self.var_type {
            Ok(vt.clone())
        } else {
            match &self.expression {
                Some(expr) => {
                    expr.deduce_type(&symtable, path, &Type::Void)
                },
                None => Ok(Type::Void),

            }
        }
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for VarDeclNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        if let Some(expr) = &self.expression {
            let symbol = err_with_location(self.location, gen.symtable.find_symbol(path, &self.name))?;
            let addr = *err_with_location(self.location, gen.addrtable.find_symbol(path, &self.name))?;
            let resolved_type = err_with_location(self.location, gen.symtable.resolve_type_alias(path, symbol.data_type.clone()))?;

            let value = expr.generate_casted(gen, path, function, &resolved_type, &ValueType::RValue)?;
            if symbol.data_type.is_int_type() {
                let int_value = basic_value_to_int(self.location, value.as_ref())?;
                wrap_err(self.location, gen.builder.build_store(addr, int_value))?;
            } else if symbol.data_type == Type::RawPtr {
                let ptr_value = basic_value_to_ptr(self.location, value.as_ref())?;
                wrap_err(self.location, gen.builder.build_store(addr, ptr_value))?;
            } else {
                compiler_err!(self.location, "unsupported variable type")
            }
        }
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let var_type = self.deduce_type(path, symtable)?;
        err_with_location(self.location, symtable.add_symbol(path, SymbolInfo{name: self.name.clone(), sym_type: SymbolType::LocalVariable, data_type: var_type, location: self.location}))?;
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::VarDeclNode(self)
    }
}

#[derive(Debug)]
pub struct RefDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub expression: ExpressionBox<'ctx, 'st>,
    pub var_type: Option<Type>,
}

impl<'ctx, 'st> RefDeclNode<'ctx, 'st> {
    fn deduce_type(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<Type> {
        if let Some(vt) = &self.var_type {
            Ok(vt.clone())
        } else {
            self.expression.deduce_type(&symtable, path, &Type::Void)
        }
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for RefDeclNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let symbol = err_with_location(self.location, gen.symtable.find_symbol(path, &self.name))?;
        let addr = *err_with_location(self.location, gen.addrtable.find_symbol(path, &self.name))?;
        let resolved_type = err_with_location(self.location, gen.symtable.resolve_type_alias(path, symbol.data_type.clone()))?;

        let value = self.expression.generate_il(gen, path, function, &resolved_type, &ValueType::LValue)?;
        let ptr_value = basic_value_to_ptr(self.location, value.as_ref())?;
        wrap_err(self.location, gen.builder.build_store(addr, ptr_value))?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let var_type = self.deduce_type(path, symtable)?;
        err_with_location(self.location, symtable.add_symbol(path, SymbolInfo{name: self.name.clone(), sym_type: SymbolType::LocalReference, data_type: var_type, location: self.location}))?;
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::RefDeclNode(self)
    }
}

#[derive(Debug)]
pub struct IfNode<'ctx, 'st> {
    pub location: Location,
    pub condition: ExpressionBox<'ctx, 'st>,
    pub then_scope: ScopeNode<'ctx, 'st>,
    pub else_scope: Option<ScopeNode<'ctx, 'st>>,
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for IfNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>) -> CompilerResult<()> {
        let current_block = wrap_option(self.location, gen.builder.get_insert_block(), "statement not located in a valid block")?;

        let ifthen = self.then_scope.generate_il(gen, &path.sub(&self.then_scope.name), function)?;
        let ifend = gen.context.append_basic_block(*function, "if.end");
        wrap_err(self.location, gen.builder.build_unconditional_branch(ifend))?;

        let ifelse = if let Some(else_scope) = &self.else_scope {
            let scope = else_scope.generate_il(gen, &path.sub(&else_scope.name), function)?;
            wrap_err(self.location, gen.builder.build_unconditional_branch(ifend))?;
            scope
        } else {
            ifend
        };

        gen.builder.position_at_end(current_block);
        self.condition.generate_boolean(gen, path, function, ifthen, ifelse)?;

        gen.builder.position_at_end(ifend);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        self.then_scope.collect_symbols(&path.sub(&self.then_scope.name), symtable)?;
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
        let while_cond = gen.context.append_basic_block(*function, "while.cond");
        wrap_err(self.location, gen.builder.build_unconditional_branch(while_cond))?;
        gen.builder.position_at_end(while_cond);

        let while_body = self.scope.generate_il(gen, &path.sub(&self.scope.name), function)?;
        wrap_err(self.location, gen.builder.build_unconditional_branch(while_cond))?;

        let while_end = gen.context.append_basic_block(*function, "while.end");

        gen.builder.position_at_end(while_cond);
        self.condition.generate_boolean(gen, path, function, while_body, while_end)?;

        gen.builder.position_at_end(while_end);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        self.scope.collect_symbols(&path.sub(&self.scope.name), symtable)?;
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
            SymbolType::FunctionArg(index, is_ref) => {
                let arg_expr = wrap_option(symbol.location, function.get_nth_param(index as u32), "invalid function argument")?;
                if is_ref {
                    let arg_ptr = basic_value_to_ptr(symbol.location, &arg_expr)?;
                    match *value_type {
                        ValueType::LValue => Ok(Box::new(arg_ptr)),
                        ValueType::RValue => Ok(Box::new(gen.load_var(self.location, &symbol.data_type, &arg_ptr, self.name.as_ref())?)),
                        ValueType::None => Ok(gen.null_ptr()),
                    }
                } else {
                    Ok(Box::new(arg_expr))
                }
            },
            SymbolType::LocalVariable => {
                let (sym, ptr) = gen.find_symbol_with_addr(self.location, path, self.name.as_ref())?;
                match *value_type {
                    ValueType::LValue => Ok(Box::new(*ptr)),
                    ValueType::RValue => Ok(Box::new(gen.load_var(self.location, &sym.data_type, ptr, self.name.as_ref())?)),
                    ValueType::None => Ok(gen.null_ptr()),
                }
            },
            SymbolType::LocalReference => {
                let (sym, ptr) = gen.find_symbol_with_addr(self.location, path, self.name.as_ref())?;
                let loaded_ptr_var = gen.load_var(self.location, &Type::RawPtr, ptr, self.name.as_ref())?;
                let loaded_ptr = basic_value_to_ptr(self.location, &loaded_ptr_var)?;
                match *value_type {
                    ValueType::LValue => Ok(Box::new(loaded_ptr)),
                    ValueType::RValue => Ok(Box::new(gen.load_var(self.location, &sym.data_type, &loaded_ptr, self.name.as_ref())?)),
                    ValueType::None => Ok(gen.null_ptr()),
                }
            },
            SymbolType::ConstantDef => {
                let basic_val = wrap_option(self.location, gen.addrtable.find_basic_value(path, &self.name), "unable to find value")?;
                Ok(Box::new(basic_val.as_basic_value_enum()))
            },
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
pub struct NullNode {
    pub location: Location,
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for NullNode {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, _: &SymbolPath, _: &FunctionValue<'ctx>, _: &Type, _: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        Ok(gen.null_ptr())
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(Type::RawPtr)
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

        self.to_constexpr_value(gen, path, expected_type)
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        if expected_type.is_int_type() && !expected_type.is_bool_type() {
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

    fn to_constexpr_value(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, expected_type: &Type) -> CompilerResult<BasicValueBox<'ctx>> {
        let num_type = self.deduce_type(&gen.symtable, path, expected_type)?;
        let llvm_type = wrap_option(self.location, num_type.to_llvm_type(gen.context), "unable to map llvm type")?;
        if let AnyTypeEnum::IntType(it) = llvm_type {
            Ok(Box::new(it.const_int(self.number, true)))
        } else {
            compiler_err!(self.location, "tried assiging number to a non int type");
        }
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

impl<'ctx, 'st> BinaryExpressionNode<'ctx, 'st> {
    fn deduce_left_value_type(&self) -> ValueType {
       match self.operation {
        BinaryOperation::Assign => ValueType::LValue,
        _ => ValueType::RValue,
       }
    }

    fn deduce_operand_and_out_type(&self, symtable: &SymbolTable, path: &SymbolPath, expected_type: &Type) -> CompilerResult<(Type, Type)> {
        let left_deduced_type = self.left.deduce_type(symtable, path, expected_type)?;
        let right_deduced_type = self.right.deduce_type(symtable, path, expected_type)?;

        if self.operation == BinaryOperation::Assign {
            if left_deduced_type.is_void() {
                Ok((right_deduced_type.clone(), right_deduced_type))
            } else {
                Ok((left_deduced_type.clone(), left_deduced_type))
            }
        } else {
            let wider_type = left_deduced_type.wider_type(&right_deduced_type);
            if self.operation.is_predicate() {
                Ok((wider_type, Type::Bool))
            } else {
                Ok((wider_type.clone(), wider_type))
            }
        }
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for BinaryExpressionNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret assignment expression as an l-value");
        }

        match self.operation {
            BinaryOperation::LogicalAnd => {
                let and_block = gen.context.append_basic_block(*function, "and.pred");
                let end_block = gen.context.append_basic_block(*function, "end.pred");

                let left = self.left.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
                let left_int = basic_value_to_int(*self.get_location(), left.as_ref())?;
                let left_block = wrap_option(self.location, gen.builder.get_insert_block(), "invalid block")?;
                wrap_err(*self.get_location(), gen.builder.build_conditional_branch(left_int, and_block, end_block))?;

                gen.builder.position_at_end(and_block);

                let right = self.right.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
                let right_block = wrap_option(self.location, gen.builder.get_insert_block(), "invalid block")?;
                wrap_err(self.location, gen.builder.build_unconditional_branch(end_block))?;

                gen.builder.position_at_end(end_block);

                let result = wrap_err(self.location, gen.builder.build_phi(gen.context.bool_type(), "and.result"))?;
                result.add_incoming(&[(&gen.context.bool_type().const_zero(), left_block), (right.as_ref(), right_block)]);

                return Ok(Box::new(result.as_basic_value()));
            },
            BinaryOperation::LogicalOr => {
                let or_block = gen.context.append_basic_block(*function, "or.pred");
                let end_block = gen.context.append_basic_block(*function, "end.pred");

                let left = self.left.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
                let left_int = basic_value_to_int(*self.get_location(), left.as_ref())?;
                let left_block = wrap_option(self.location, gen.builder.get_insert_block(), "invalid block")?;
                wrap_err(*self.get_location(), gen.builder.build_conditional_branch(left_int, end_block, or_block))?;

                gen.builder.position_at_end(or_block);

                let right = self.right.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
                let right_block = wrap_option(self.location, gen.builder.get_insert_block(), "invalid block")?;
                wrap_err(self.location, gen.builder.build_unconditional_branch(end_block))?;

                gen.builder.position_at_end(end_block);

                let result = wrap_err(self.location, gen.builder.build_phi(gen.context.bool_type(), "or.result"))?;
                result.add_incoming(&[(&gen.context.bool_type().const_int(1, false), left_block), (right.as_ref(), right_block)]);

                return Ok(Box::new(result.as_basic_value()));
            },
            _ => { },
        };

        // For assigment we always expect the left hand side to be an l-value.
        let left_value_type = self.deduce_left_value_type();
        let (operand_type, _) = self.deduce_operand_and_out_type(&gen.symtable, path, expected_type)?;

        let left = self.left.generate_casted(gen, path, function, &operand_type, &left_value_type)?;
        let right = self.right.generate_casted(gen, path, function, &operand_type, &ValueType::RValue)?;

        self.operation.build(gen, &self.location, &operand_type, left.as_ref(), right.as_ref())
    }

    fn generate_boolean(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, true_block: BasicBlock<'ctx>, false_block: BasicBlock<'ctx>) -> CompilerResult<()> {
        match self.operation {
            BinaryOperation::LogicalAnd => {
                let and_block = gen.context.append_basic_block(*function, "and.pred");
                self.left.generate_boolean(gen, path, function, and_block, false_block)?;
                gen.builder.position_at_end(and_block);
                self.right.generate_boolean(gen, path, function, true_block, false_block)?;
            }
            BinaryOperation::LogicalOr => {
                let or_block = gen.context.append_basic_block(*function, "or.pred");
                self.left.generate_boolean(gen, path, function, true_block, or_block)?;
                gen.builder.position_at_end(or_block);
                self.right.generate_boolean(gen, path, function, true_block, false_block)?;
            }
            _ => {
                self.build_boolean_branch(gen, path, function, true_block, false_block)?;
            }
        };

        Ok(())
    }

    fn deduce_type(&self, symtable: &SymbolTable, path: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        let (_, out_type) = self.deduce_operand_and_out_type(symtable, path, expected_type)?;
        Ok(out_type)
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
            SingularOperation::Not => {
                let expr_value = self.expr.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
                let expr_int = basic_value_to_int(self.location, expr_value.as_ref())?;
                let result = wrap_err(self.location, gen.builder.build_xor(expr_int, gen.context.bool_type().const_int(1, false), "not.result"))?;
                Ok(Box::new(result))
            },
            SingularOperation::Minus => {
                let expr_value = self.expr.generate_casted(gen, path, function, expected_type, &ValueType::RValue)?;
                let expr_int = basic_value_to_int(self.location, expr_value.as_ref())?;
                let result = wrap_err(self.location, gen.builder.build_int_sub(gen.context.i32_type().const_int(0, false), expr_int, "minus.result"))?;
                Ok(Box::new(result))
            },
        }
    }

    fn generate_boolean(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, true_block: BasicBlock<'ctx>, false_block: BasicBlock<'ctx>) -> CompilerResult<()> {
        match self.operation {
            SingularOperation::Not => {
                self.expr.generate_boolean(gen, path, function, false_block, true_block)?;
            },
            _ => {
                self.build_boolean_branch(gen, path, function, true_block, false_block)?;
            }
        };

        Ok(())
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        match self.operation {
            SingularOperation::AddressOf => {
                Ok(Type::RawPtr)
            },
            SingularOperation::Deference => {
                Ok(expected_type.clone())
            },
            SingularOperation::Not => {
                Ok(Type::Bool)
            },
            SingularOperation::Minus => {
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
                let arg_value = if arg_type.is_ref {
                    arg_expr.generate_il(gen, path, function, &arg_type.arg_type, &ValueType::LValue)?
                } else {
                    arg_expr.generate_il(gen, path, function, &arg_type.arg_type, &ValueType::RValue)?
                };

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
        let index_type = self.index.deduce_type(&gen.symtable, path, expected_type)?;
        let index_value = self.index.generate_il(gen, path, function, &index_type, &ValueType::RValue)?;
        let index = basic_value_to_int(self.location, index_value.as_ref())?;

        let indexed_ptr = gen.build_get_element_ptr(self.location, expected_type, obj_ptr, &[index], "addrindex")?;
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
        let expr = self.expr.generate_il(gen, path, function, &source_type, &ValueType::RValue)?;
        
        gen.build_cast(self.location, &source_type, &self.target_type, expr)
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(self.target_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct GetFieldNode<'ctx, 'st> {
    pub location: Location,
    pub object_expr: ExpressionBox<'ctx, 'st>,
    pub field_name: String,
}

impl<'ctx, 'st> GetFieldNode<'ctx, 'st> {
    fn get_symbol<'a>(&self, symtable: &'a SymbolTable, path: &SymbolPath, expected_type: &Type) -> CompilerResult<&'a SymbolInfo> {
        let obj_type = self.object_expr.deduce_type(symtable, path, expected_type)?;
        if let Type::Alias(alias) = &obj_type {
            let alias_path = err_with_location(self.location, symtable.find_symbol_path(path, alias))?;
            let symbol = err_with_location(self.location, symtable.find_symbol(&alias_path, &self.field_name))?;
            Ok(symbol)
        } else {
            compiler_err!(self.location, "invalid object type");
        }
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for GetFieldNode<'ctx, 'st> {
    fn generate_il(&self, gen: &mut IlGenerator<'ctx, 'st>, path: &SymbolPath, function: &FunctionValue<'ctx>, expected_type: &Type, value_type: &ValueType) -> CompilerResult<BasicValueBox<'ctx>> {
        let obj_type = self.object_expr.deduce_type(gen.symtable, path, expected_type)?;
        let obj_resolved = err_with_location(self.location, gen.symtable.resolve_type_alias(path, obj_type))?;
        let obj = self.object_expr.generate_il(gen, path, function, &obj_resolved, &ValueType::LValue)?;
        let ptr = basic_value_to_ptr(self.location, obj.as_ref())?;
        let sym = self.get_symbol(gen.symtable, path, expected_type)?;

        if let SymbolType::StructField(id) = sym.sym_type {
            let field_ptr = gen.build_get_element_ptr(self.location, &obj_resolved, ptr, &[gen.context.i32_type().const_int(0, true), gen.context.i32_type().const_int(id as u64, true)], &self.field_name)?;
            match value_type {
                ValueType::LValue => { Ok(Box::new(field_ptr)) },
                ValueType::RValue => {
                    Ok(Box::new(gen.load_var(self.location, &sym.data_type, &field_ptr, "structfield")?))
                },
                _ => compiler_err!(self.location, "invalid symbol"),

            }
        } else {
            compiler_err!(self.location, "invalid symbol");
        }
    }

    fn deduce_type(&self, symtable: &SymbolTable, path: &SymbolPath, expected_type: &Type) -> CompilerResult<Type> {
        let sym = self.get_symbol(symtable, path, expected_type)?;
        Ok(sym.data_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}
