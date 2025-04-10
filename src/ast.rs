use crate::error::{CompilerResult, CompilerResultErrorMapper};
use crate::ir_build_context::{BasicValueExtension, IRBuildContext};
use crate::symbols::{SymbolPath, SymbolTable};
use crate::token::{Location, OperatorType};
use crate::typing::{Type, ValueType};

use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::values::{BasicValue, FunctionValue};
use inkwell::IntPredicate;

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
            _ => None,
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

    pub fn precedence(&self) -> i32 {
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
        self.precedence() >= other.precedence()
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
            _ => None,
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
            Some(AnyOperation::Singular(SingularOperation::from_op_type(
                op_type,
            )?))
        } else {
            Some(AnyOperation::Binary(BinaryOperation::from_op_type(
                op_type,
            )?))
        }
    }

    pub fn proceeds(&self, other: &AnyOperation) -> bool {
        match self {
            AnyOperation::Singular(_) => true,
            AnyOperation::Binary(self_bin) => match other {
                AnyOperation::Singular(_) => false,
                AnyOperation::Binary(other_bin) => self_bin.proceeds(other_bin),
                AnyOperation::None => false,
            },
            AnyOperation::None => false,
        }
    }
}

pub type BasicValueBox<'ctx> = Box<dyn BasicValue<'ctx> + 'ctx>;

pub trait GlobalStatementNode<'ctx, 'st>: std::fmt::Debug {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
    ) -> CompilerResult<()>;
    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()>;
}

pub trait StatementNode<'ctx, 'st>: std::fmt::Debug {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()>;
    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()>;
    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st>;
}

pub trait ExpressionNode<'ctx, 'st>: std::fmt::Debug {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>>;
    fn deduce_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<Type>;
    fn get_location(&self) -> &Location;

    fn build_boolean_branch(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        let value = self.generate_casted(gen, path, function, &Type::Bool, &ValueType::RValue)?;
        let value_int = value.as_ref().to_int(*self.get_location())?;
        gen.builder
            .build_conditional_branch(value_int, true_block, false_block)
            .to_comp_res(*self.get_location())?;
        Ok(())
    }

    fn generate_boolean(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        self.build_boolean_branch(gen, path, function, true_block, false_block)
    }

    fn generate_casted(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let stmt = self.generate(gen, path, function, expected_type, value_type)?;
        if *value_type == ValueType::LValue {
            Ok(stmt)
        } else {
            let deduced_type = self.deduce_type(&gen.symtable, path, expected_type)?;
            gen.build_cast(*self.get_location(), &deduced_type, expected_type, stmt)
        }
    }

    fn to_constexpr_value(
        &self,
        _: &mut IRBuildContext<'ctx, 'st>,
        _: &SymbolPath,
        _: &Type,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        compiler_err!(
            *self.get_location(),
            "expression cannot be resolved at compile time"
        );
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

trait StatementVisitor<'ctx, 'st> {
    type Payload;

    fn visit_return_node(&self, node: &ReturnNode<'ctx, 'st>, pd: &Self::Payload);
    fn visit_var_decl_node(&self, node: &VarDeclNode<'ctx, 'st>, pd: &Self::Payload);
    fn visit_ref_decl_node(&self, node: &RefDeclNode<'ctx, 'st>, pd: &Self::Payload);
    fn visit_if_node(&self, node: &IfNode<'ctx, 'st>, pd: &Self::Payload);
    fn visit_while_node(&self, node: &WhileNode<'ctx, 'st>, pd: &Self::Payload);
    fn visit_expression_statement_node(&self, node: &ExpressionStatementNode<'ctx, 'st>, pd: &Self::Payload);
}

pub type ExpressionBox<'ctx, 'st> = Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>;

// **********************************
// ******** GLOBAL STATEMENTS *******
// **********************************

#[derive(Debug)]
pub struct SourceUnit<'ctx, 'st> {
    pub body: Vec<Box<dyn GlobalStatementNode<'ctx, 'st> + 'ctx>>,
}

#[derive(Debug)]
pub struct ScopeNode<'ctx, 'st> {
    pub body: Vec<Box<dyn StatementNode<'ctx, 'st> + 'ctx>>,
    pub name: String,
}

#[derive(Debug)]
pub struct ConstDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub const_type: Option<Type>,
    pub value: ExpressionBox<'ctx, 'st>,
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
    pub self_type: Option<Type>,
    pub name: String,
    pub params: Vec<FunctionArg>,
    pub ret_type: Type,
    pub linkage: FunctionLinkage,
    pub scope: Option<ScopeNode<'ctx, 'st>>,
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

#[derive(Debug)]
pub struct ImportNode {
    pub location: Location,
    pub module_name: String,
}

// **********************************
// ********** STATEMENTS ************
// **********************************

#[derive(Debug)]
pub struct ReturnNode<'ctx, 'st> {
    pub location: Location,
    pub expression: Option<ExpressionBox<'ctx, 'st>>,
}

#[derive(Debug)]
pub struct VarDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub expression: Option<ExpressionBox<'ctx, 'st>>,
    pub var_type: Option<Type>,
}

#[derive(Debug)]
pub struct RefDeclNode<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub expression: ExpressionBox<'ctx, 'st>,
    pub var_type: Option<Type>,
}

#[derive(Debug)]
pub struct IfNode<'ctx, 'st> {
    pub location: Location,
    pub condition: ExpressionBox<'ctx, 'st>,
    pub then_scope: ScopeNode<'ctx, 'st>,
    pub else_scope: Option<ScopeNode<'ctx, 'st>>,
}

#[derive(Debug)]
pub struct WhileNode<'ctx, 'st> {
    pub location: Location,
    pub condition: ExpressionBox<'ctx, 'st>,
    pub scope: ScopeNode<'ctx, 'st>,
}

#[derive(Debug)]
pub struct ExpressionStatementNode<'ctx, 'st> {
    pub expression: ExpressionBox<'ctx, 'st>,
}

// **********************************
// ********** EXPRESSIONS ***********
// **********************************

#[derive(Debug)]
pub struct IdentifierNode {
    pub location: Location,
    pub name: String,
}

#[derive(Debug)]
pub struct NullNode {
    pub location: Location,
}

#[derive(Debug)]
pub struct SelfNode {
    pub location: Location,
}

#[derive(Debug)]
pub struct NumberNode {
    pub location: Location,
    pub number: u64,
}

#[derive(Debug)]
pub struct StringNode {
    pub location: Location,
    pub value: String,
}

#[derive(Debug)]
pub struct BinaryExpressionNode<'ctx, 'st> {
    pub location: Location,
    pub operation: BinaryOperation,
    pub left: ExpressionBox<'ctx, 'st>,
    pub right: ExpressionBox<'ctx, 'st>,
}

#[derive(Debug)]
pub struct SingularExpressionNode<'ctx, 'st> {
    pub location: Location,
    pub operation: SingularOperation,
    pub expr: ExpressionBox<'ctx, 'st>,
}

#[derive(Debug)]
pub struct FunctionCall<'ctx, 'st> {
    pub location: Location,
    pub name: String,
    pub args: Vec<ExpressionBox<'ctx, 'st>>,
}

#[derive(Debug)]
pub struct GetElementNode<'ctx, 'st> {
    pub location: Location,
    pub object: ExpressionBox<'ctx, 'st>,
    pub index: ExpressionBox<'ctx, 'st>,
}

#[derive(Debug)]
pub struct CastNode<'ctx, 'st> {
    pub location: Location,
    pub target_type: Type,
    pub expr: ExpressionBox<'ctx, 'st>,
}

#[derive(Debug)]
pub struct GetFieldNode<'ctx, 'st> {
    pub location: Location,
    pub object_expr: ExpressionBox<'ctx, 'st>,
    pub field_name: String,
}

#[derive(Debug)]
pub struct MethodCall<'ctx, 'st> {
    pub location: Location,
    pub object_expr: ExpressionBox<'ctx, 'st>,
    pub name: String,
    pub args: Vec<ExpressionBox<'ctx, 'st>>,
}
