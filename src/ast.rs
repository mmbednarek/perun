use crate::error::{CompilerResult, CompilerResultErrorMapper};
use crate::symbols::{SymbolInfo, SymbolPath, SymbolTable, SymbolType};
use crate::token::{Location, OperatorType};
use crate::typing::{DataSize, Identifier, Type, ValueType};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MathBinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignmentBinaryOperation {
    Assign,
    Math(MathBinaryOperation),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComparisonBinaryOperation {
    Equals,
    NotEquals,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LogicalBinaryOperation {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    Assignment(AssignmentBinaryOperation),
    Math(MathBinaryOperation),
    Comparison(ComparisonBinaryOperation),
    Logical(LogicalBinaryOperation),
}

impl BinaryOperation {
    pub fn from_op_type(op_type: OperatorType) -> Option<BinaryOperation> {
        match op_type {
            OperatorType::Plus => Some(BinaryOperation::Math(MathBinaryOperation::Add)),
            OperatorType::Minus => Some(BinaryOperation::Math(MathBinaryOperation::Subtract)),
            OperatorType::Asterisk => Some(BinaryOperation::Math(MathBinaryOperation::Multiply)),
            OperatorType::Slash => Some(BinaryOperation::Math(MathBinaryOperation::Divide)),
            OperatorType::Less => {
                Some(BinaryOperation::Comparison(ComparisonBinaryOperation::Less))
            }
            OperatorType::LessOrEqual => Some(BinaryOperation::Comparison(
                ComparisonBinaryOperation::LessOrEqual,
            )),
            OperatorType::Greater => Some(BinaryOperation::Comparison(
                ComparisonBinaryOperation::Greater,
            )),
            OperatorType::GreaterOrEqual => Some(BinaryOperation::Comparison(
                ComparisonBinaryOperation::GreaterOrEqual,
            )),
            OperatorType::EqualsEquals => Some(BinaryOperation::Comparison(
                ComparisonBinaryOperation::Equals,
            )),
            OperatorType::NotEquals => Some(BinaryOperation::Comparison(
                ComparisonBinaryOperation::NotEquals,
            )),
            OperatorType::Equals => Some(BinaryOperation::Assignment(
                AssignmentBinaryOperation::Assign,
            )),
            OperatorType::Percent => Some(BinaryOperation::Math(MathBinaryOperation::Modulo)),
            OperatorType::LogicalAnd => {
                Some(BinaryOperation::Logical(LogicalBinaryOperation::LogicalAnd))
            }
            OperatorType::LogicalOr => {
                Some(BinaryOperation::Logical(LogicalBinaryOperation::LogicalOr))
            }
            OperatorType::Increase => Some(BinaryOperation::Assignment(
                AssignmentBinaryOperation::Math(MathBinaryOperation::Add),
            )),
            OperatorType::Decrease => Some(BinaryOperation::Assignment(
                AssignmentBinaryOperation::Math(MathBinaryOperation::Subtract),
            )),
            OperatorType::MultiplyAssign => Some(BinaryOperation::Assignment(
                AssignmentBinaryOperation::Math(MathBinaryOperation::Multiply),
            )),
            OperatorType::DivideAssign => Some(BinaryOperation::Assignment(
                AssignmentBinaryOperation::Math(MathBinaryOperation::Divide),
            )),
            OperatorType::ModuloAssign => Some(BinaryOperation::Assignment(
                AssignmentBinaryOperation::Math(MathBinaryOperation::Modulo),
            )),
            _ => None,
        }
    }

    pub fn is_predicate(&self) -> bool {
        match self {
            BinaryOperation::Comparison(_) => true,
            BinaryOperation::Logical(_) => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> i32 {
        match self {
            BinaryOperation::Math(MathBinaryOperation::Add) => 5,
            BinaryOperation::Math(MathBinaryOperation::Subtract) => 5,
            BinaryOperation::Math(MathBinaryOperation::Divide) => 6,
            BinaryOperation::Math(MathBinaryOperation::Multiply) => 6,
            BinaryOperation::Math(MathBinaryOperation::Modulo) => 6,
            BinaryOperation::Comparison(ComparisonBinaryOperation::Less) => 4,
            BinaryOperation::Comparison(ComparisonBinaryOperation::LessOrEqual) => 4,
            BinaryOperation::Comparison(ComparisonBinaryOperation::Greater) => 4,
            BinaryOperation::Comparison(ComparisonBinaryOperation::GreaterOrEqual) => 4,
            BinaryOperation::Comparison(ComparisonBinaryOperation::Equals) => 4,
            BinaryOperation::Comparison(ComparisonBinaryOperation::NotEquals) => 4,
            BinaryOperation::Logical(LogicalBinaryOperation::LogicalAnd) => 3,
            BinaryOperation::Logical(LogicalBinaryOperation::LogicalOr) => 2,
            BinaryOperation::Assignment(AssignmentBinaryOperation::Assign) => 1,
            BinaryOperation::Assignment(AssignmentBinaryOperation::Math(
                MathBinaryOperation::Add,
            )) => 1,
            BinaryOperation::Assignment(AssignmentBinaryOperation::Math(
                MathBinaryOperation::Subtract,
            )) => 1,
            BinaryOperation::Assignment(AssignmentBinaryOperation::Math(
                MathBinaryOperation::Multiply,
            )) => 1,
            BinaryOperation::Assignment(AssignmentBinaryOperation::Math(
                MathBinaryOperation::Divide,
            )) => 1,
            BinaryOperation::Assignment(AssignmentBinaryOperation::Math(
                MathBinaryOperation::Modulo,
            )) => 1,
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

pub trait LocatedNode {
    fn get_location(&self) -> &Location;
}

pub type GlobalStatementBox = Box<AnyGlobalStatement>;
pub type StatementBox = Box<AnyStatementNode>;
pub type ExpressionBox = Box<AnyExpressionNode>;

// **********************************
// ******** GLOBAL STATEMENTS *******
// **********************************

#[derive(Debug, Clone)]
pub struct SourceUnit {
    pub body: Vec<Box<AnyGlobalStatement>>,
}

impl Into<AnyGlobalStatement> for &SourceUnit {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::SourceUnit(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ConstDeclNode {
    pub location: Location,
    pub name: String,
    pub const_type: Option<Type>,
    pub value: ExpressionBox,
    pub is_public: bool,
}

impl LocatedNode for ConstDeclNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &ConstDeclNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::ConstDecl(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArg {
    pub location: Location,
    pub name: String,
    pub arg_type: Type,
    pub is_ref: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionLinkage {
    Standard,
    External,
    Entrypoint,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub location: Location,
    pub self_type: Option<Type>,
    pub name: String,
    pub params: Vec<FunctionArg>,
    pub ret_type: Type,
    pub linkage: FunctionLinkage,
    pub scope: Option<ScopeNode>,
    pub symbol_override: Option<String>,
    pub is_public: bool,
}

impl<'ctx, 'st> FunctionNode {
    pub fn sub_path(
        &self,
        location: Location,
        symbol_table: &SymbolTable,
        path: &SymbolPath,
    ) -> CompilerResult<SymbolPath> {
        if let Some(self_type) = &self.self_type {
            match self_type {
                Type::Alias(alias) => {
                    let alias_path = symbol_table
                        .get_identifier_path(path, alias)
                        .to_comp_res(location)?;
                    Ok(alias_path.sub(&self.name))
                }
                _ => compiler_err!(self.location, "invalid type"),
            }
        } else {
            match self.linkage {
                FunctionLinkage::Standard | FunctionLinkage::Entrypoint => Ok(path.sub(&self.name)),
                FunctionLinkage::External => Ok(SymbolPath::empty().sub(&self.name)),
            }
        }
    }

    pub fn effective_name(&self, path: &SymbolPath) -> String {
        assert!(!path.is_empty());
        if let Some(symbol) = &self.symbol_override {
            return symbol.clone();
        }

        match self.linkage {
            FunctionLinkage::Standard => format!("perun.fn.{}", path.to_string()),
            FunctionLinkage::External => path.to_string(),
            FunctionLinkage::Entrypoint => "main".to_string(),
        }
    }
}

impl LocatedNode for FunctionNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &FunctionNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Function(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub location: Location,
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<StructField>,
    pub is_public: bool,
}

impl LocatedNode for StructNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &StructNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Struct(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct EnumNode {
    pub location: Location,
    pub name: String,
    pub enumerations: Vec<String>,
    pub is_public: bool,
}

impl LocatedNode for EnumNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &EnumNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Enum(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ImportNode {
    pub location: Location,
    pub module_name: String,
    pub dst_path: Option<SymbolPath>,
    pub is_public: bool,
}

impl LocatedNode for ImportNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &ImportNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Import(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct AliasNode {
    pub location: Location,
    pub is_public: bool,
    pub name: String,
    pub aliased_type: Type,
}

impl LocatedNode for AliasNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &AliasNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Alias(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct UnionNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<StructField>,
    pub is_public: bool,
}

impl LocatedNode for UnionNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &UnionNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Union(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ModuleNode {
    pub location: Location,
    pub name: String,
}

impl LocatedNode for ModuleNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyGlobalStatement> for &ModuleNode {
    fn into(self) -> AnyGlobalStatement {
        AnyGlobalStatement::Module(self.clone())
    }
}

#[derive(Debug, Clone)]
pub enum AnyGlobalStatement {
    SourceUnit(SourceUnit),
    ConstDecl(ConstDeclNode),
    Function(FunctionNode),
    Struct(StructNode),
    Import(ImportNode),
    Enum(EnumNode),
    Alias(AliasNode),
    Union(UnionNode),
    Module(ModuleNode),
}

pub trait GlobalStatementVisitor {
    type Payload;
    type VisitResult;

    fn visit_source_unit(&mut self, node: &SourceUnit, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_const_decl(&mut self, node: &ConstDeclNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_function(&mut self, node: &FunctionNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_struct(&mut self, node: &StructNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_import(&mut self, node: &ImportNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_enum(&mut self, node: &EnumNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_alias(&mut self, node: &AliasNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_union(&mut self, node: &UnionNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_module(&mut self, node: &ModuleNode, pd: &Self::Payload) -> Self::VisitResult;

    fn visit_global_statement(
        &mut self,
        node: &AnyGlobalStatement,
        pd: &Self::Payload,
    ) -> Self::VisitResult {
        match node {
            AnyGlobalStatement::SourceUnit(node) => self.visit_source_unit(node, pd),
            AnyGlobalStatement::ConstDecl(node) => self.visit_const_decl(node, pd),
            AnyGlobalStatement::Function(node) => self.visit_function(node, pd),
            AnyGlobalStatement::Struct(node) => self.visit_struct(node, pd),
            AnyGlobalStatement::Import(node) => self.visit_import(node, pd),
            AnyGlobalStatement::Enum(node) => self.visit_enum(node, pd),
            AnyGlobalStatement::Alias(node) => self.visit_alias(node, pd),
            AnyGlobalStatement::Union(node) => self.visit_union(node, pd),
            AnyGlobalStatement::Module(node) => self.visit_module(node, pd),
        }
    }
}

// **********************************
// ********** STATEMENTS ************
// **********************************

#[derive(Debug, Clone)]
pub struct ScopeNode {
    pub body: Vec<Box<AnyStatementNode>>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub location: Location,
    pub expression: Option<ExpressionBox>,
}

impl LocatedNode for ReturnNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyStatementNode> for &ReturnNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::ReturnNode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct VarDeclNode {
    pub location: Location,
    pub name: String,
    pub expression: Option<ExpressionBox>,
    pub var_type: Option<Type>,
}

impl LocatedNode for VarDeclNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyStatementNode> for &VarDeclNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::VarDeclNode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct RefDeclNode {
    pub location: Location,
    pub name: String,
    pub expression: ExpressionBox,
    pub var_type: Option<Type>,
}

impl LocatedNode for RefDeclNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyStatementNode> for &RefDeclNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::RefDeclNode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub location: Location,
    pub condition: ExpressionBox,
    pub then_scope: ScopeNode,
    pub else_scope: Option<ScopeNode>,
}

impl LocatedNode for IfNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyStatementNode> for &IfNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::IfNode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub location: Location,
    pub condition: ExpressionBox,
    pub scope: ScopeNode,
}

impl LocatedNode for WhileNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyStatementNode> for &WhileNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::WhileNode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementNode {
    pub expression: ExpressionBox,
}

impl LocatedNode for ExpressionStatementNode {
    fn get_location(&self) -> &Location {
        self.expression.get_location()
    }
}

impl Into<AnyStatementNode> for &ExpressionStatementNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::ExpressionStatementNode(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub condition: ExpressionBox,
    pub scope: ScopeNode,
}

#[derive(Debug, Clone)]
pub struct MatchNode {
    pub location: Location,
    pub expression: ExpressionBox,
    pub cases: Vec<MatchCase>,
    pub default_case: Option<ScopeNode>,
}

impl LocatedNode for MatchNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyStatementNode> for &MatchNode {
    fn into(self) -> AnyStatementNode {
        AnyStatementNode::MatchStatement(self.clone())
    }
}

#[derive(Debug, Clone)]
pub enum AnyStatementNode {
    ReturnNode(ReturnNode),
    VarDeclNode(VarDeclNode),
    RefDeclNode(RefDeclNode),
    IfNode(IfNode),
    WhileNode(WhileNode),
    ExpressionStatementNode(ExpressionStatementNode),
    MatchStatement(MatchNode),
}

impl LocatedNode for AnyStatementNode {
    fn get_location(&self) -> &Location {
        match self {
            AnyStatementNode::ReturnNode(node) => node.get_location(),
            AnyStatementNode::VarDeclNode(node) => node.get_location(),
            AnyStatementNode::RefDeclNode(node) => node.get_location(),
            AnyStatementNode::IfNode(node) => node.get_location(),
            AnyStatementNode::WhileNode(node) => node.get_location(),
            AnyStatementNode::ExpressionStatementNode(node) => node.get_location(),
            AnyStatementNode::MatchStatement(node) => node.get_location(),
        }
    }
}

pub trait StatementVisitor {
    type Payload;
    type VisitResult;

    fn visit_return_node(&mut self, node: &ReturnNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_var_decl_node(&mut self, node: &VarDeclNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_ref_decl_node(&mut self, node: &RefDeclNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_if_node(&mut self, node: &IfNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_while_node(&mut self, node: &WhileNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_match_node(&mut self, node: &MatchNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_expression_statement_node(
        &mut self,
        node: &ExpressionStatementNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult;

    fn visit_statement(
        &mut self,
        any_node: &AnyStatementNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult {
        match any_node {
            AnyStatementNode::ReturnNode(node) => self.visit_return_node(node, pd),
            AnyStatementNode::VarDeclNode(node) => self.visit_var_decl_node(node, pd),
            AnyStatementNode::RefDeclNode(node) => self.visit_ref_decl_node(node, pd),
            AnyStatementNode::IfNode(node) => self.visit_if_node(node, pd),
            AnyStatementNode::WhileNode(node) => self.visit_while_node(node, pd),
            AnyStatementNode::MatchStatement(node) => self.visit_match_node(node, pd),
            AnyStatementNode::ExpressionStatementNode(node) => {
                self.visit_expression_statement_node(node, pd)
            }
        }
    }
}

// **********************************
// ********** EXPRESSIONS ***********
// **********************************

#[derive(Debug, Clone)]
pub struct IdentifierNode {
    pub location: Location,
    pub name: Identifier,
}

impl LocatedNode for IdentifierNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &IdentifierNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::Identifier(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct NullNode {
    pub location: Location,
}

impl LocatedNode for NullNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &NullNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::Null(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct BooleanNode {
    pub location: Location,
    pub value: bool,
}

impl LocatedNode for BooleanNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &BooleanNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::Boolean(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct SelfNode {
    pub location: Location,
}

impl LocatedNode for SelfNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &SelfNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::SelfN(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct NumberNode {
    pub location: Location,
    pub number: u64,
}

impl LocatedNode for NumberNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &NumberNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::Number(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct FloatingPointNode {
    pub location: Location,
    pub value: f64,
}

impl LocatedNode for FloatingPointNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &FloatingPointNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::FloatingPoint(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct StringNode {
    pub location: Location,
    pub value: String,
}

impl LocatedNode for StringNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &StringNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::String(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpressionNode {
    pub location: Location,
    pub operation: BinaryOperation,
    pub left: ExpressionBox,
    pub right: ExpressionBox,
}

impl BinaryExpressionNode {
    pub fn get_left_value_type(&self) -> ValueType {
        match self.operation {
            BinaryOperation::Assignment(_) => ValueType::LValue,
            _ => ValueType::RValue,
        }
    }
}

impl LocatedNode for BinaryExpressionNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &BinaryExpressionNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::BinaryExpression(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct SingularExpressionNode {
    pub location: Location,
    pub operation: SingularOperation,
    pub expr: ExpressionBox,
}

impl LocatedNode for SingularExpressionNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &SingularExpressionNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::SingularExpression(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub location: Location,
    pub name: Identifier,
    pub args: Vec<ExpressionBox>,
}

impl LocatedNode for FunctionCall {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &FunctionCall {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::FunctionCall(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct GetElementNode {
    pub location: Location,
    pub object: ExpressionBox,
    pub index: ExpressionBox,
}

impl LocatedNode for GetElementNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &GetElementNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::GetElement(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct CastNode {
    pub location: Location,
    pub target_type: Type,
    pub expr: ExpressionBox,
}

impl LocatedNode for CastNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &CastNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::Cast(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct GetFieldNode {
    pub location: Location,
    pub object_expr: ExpressionBox,
    pub field_name: String,
}

static SYMBOL_INFO_SLICE_SIZE: SymbolInfo = SymbolInfo {
    name: String::new(),
    sym_type: SymbolType::StructField(1),
    data_type: Type::Integer {
        is_signed: false,
        size: DataSize::Bits64,
    },
    location: Location { line: 0, column: 0 },
};

impl GetFieldNode {
    pub fn get_symbol<'st>(
        &self,
        location: Location,
        symbol_table: &'st SymbolTable,
        path: &SymbolPath,
        obj_type: &Type,
    ) -> CompilerResult<&'st SymbolInfo> {
        if let Type::Alias(alias) = &obj_type {
            let alias_path = symbol_table
                .get_identifier_path(path, alias)
                .to_comp_res(location)?;
            let sym_path = symbol_table
                .find_symbol_path(&alias_path, &alias.value)
                .to_comp_res(location)?;
            let symbol = symbol_table
                .find_symbol(&sym_path, &self.field_name)
                .to_comp_res(self.location)?;
            Ok(symbol)
        } else if let Type::Slice { element_type } = &obj_type {
            if self.field_name == "count" {
                return Ok(&SYMBOL_INFO_SLICE_SIZE);
            }
            compiler_err!(self.location, "invalid object type");
        } else {
            compiler_err!(self.location, "invalid object type");
        }
    }
}

impl LocatedNode for GetFieldNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &GetFieldNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::GetField(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub location: Location,
    pub object_expr: ExpressionBox,
    pub name: String,
    pub args: Vec<ExpressionBox>,
}

impl MethodCall {
    pub fn get_method_path(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        obj_type: &Type,
    ) -> CompilerResult<SymbolPath> {
        if let Type::Alias(alias) = obj_type {
            let receiver_path = symtable
                .find_identifier_path(path, alias)
                .to_comp_res(self.location)?;
            Ok(receiver_path.sub(&self.name))
        } else {
            compiler_err!(self.location, "invalid object type")
        }
    }
}

impl LocatedNode for MethodCall {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &MethodCall {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::MethodCall(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ConstructorNode {
    pub location: Location,
    pub arguments: Vec<ExpressionBox>,
}

impl LocatedNode for ConstructorNode {
    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl Into<AnyExpressionNode> for &ConstructorNode {
    fn into(self) -> AnyExpressionNode {
        AnyExpressionNode::Constructor(self.clone())
    }
}

#[derive(Debug, Clone)]
pub enum AnyExpressionNode {
    Identifier(IdentifierNode),
    Null(NullNode),
    Boolean(BooleanNode),
    SelfN(SelfNode),
    Number(NumberNode),
    FloatingPoint(FloatingPointNode),
    String(StringNode),
    BinaryExpression(BinaryExpressionNode),
    SingularExpression(SingularExpressionNode),
    FunctionCall(FunctionCall),
    GetElement(GetElementNode),
    GetField(GetFieldNode),
    Cast(CastNode),
    MethodCall(MethodCall),
    Constructor(ConstructorNode),
}

impl LocatedNode for AnyExpressionNode {
    fn get_location(&self) -> &Location {
        match self {
            AnyExpressionNode::Identifier(node) => node.get_location(),
            AnyExpressionNode::Null(node) => node.get_location(),
            AnyExpressionNode::Boolean(node) => node.get_location(),
            AnyExpressionNode::SelfN(node) => node.get_location(),
            AnyExpressionNode::Number(node) => node.get_location(),
            AnyExpressionNode::FloatingPoint(node) => node.get_location(),
            AnyExpressionNode::String(node) => node.get_location(),
            AnyExpressionNode::BinaryExpression(node) => node.get_location(),
            AnyExpressionNode::SingularExpression(node) => node.get_location(),
            AnyExpressionNode::FunctionCall(node) => node.get_location(),
            AnyExpressionNode::GetElement(node) => node.get_location(),
            AnyExpressionNode::GetField(node) => node.get_location(),
            AnyExpressionNode::Cast(node) => node.get_location(),
            AnyExpressionNode::MethodCall(node) => node.get_location(),
            AnyExpressionNode::Constructor(node) => node.get_location(),
        }
    }
}

pub trait ExpressionVisitor {
    type Payload;
    type VisitResult;

    fn visit_identifier(&self, node: &IdentifierNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_null(&self, node: &NullNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_boolean(&self, node: &BooleanNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_self(&self, node: &SelfNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_number(&self, node: &NumberNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_floating_point(
        &self,
        node: &FloatingPointNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult;
    fn visit_string(&self, node: &StringNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_binary_expression(
        &self,
        node: &BinaryExpressionNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult;
    fn visit_singular_expression(
        &self,
        node: &SingularExpressionNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult;
    fn visit_function_call(&self, node: &FunctionCall, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_get_element(&self, node: &GetElementNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_get_field(&self, node: &GetFieldNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_cast(&self, node: &CastNode, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_method_call(&self, node: &MethodCall, pd: &Self::Payload) -> Self::VisitResult;
    fn visit_constructor(&self, node: &ConstructorNode, pd: &Self::Payload) -> Self::VisitResult;

    fn visit_expression(
        &self,
        any_expression: &AnyExpressionNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult {
        match any_expression {
            AnyExpressionNode::Identifier(node) => self.visit_identifier(node, pd),
            AnyExpressionNode::Null(node) => self.visit_null(node, pd),
            AnyExpressionNode::Boolean(node) => self.visit_boolean(node, pd),
            AnyExpressionNode::SelfN(node) => self.visit_self(node, pd),
            AnyExpressionNode::Number(node) => self.visit_number(node, pd),
            AnyExpressionNode::FloatingPoint(node) => self.visit_floating_point(node, pd),
            AnyExpressionNode::String(node) => self.visit_string(node, pd),
            AnyExpressionNode::BinaryExpression(node) => self.visit_binary_expression(node, pd),
            AnyExpressionNode::SingularExpression(node) => self.visit_singular_expression(node, pd),
            AnyExpressionNode::FunctionCall(node) => self.visit_function_call(node, pd),
            AnyExpressionNode::GetElement(node) => self.visit_get_element(node, pd),
            AnyExpressionNode::GetField(node) => self.visit_get_field(node, pd),
            AnyExpressionNode::Cast(node) => self.visit_cast(node, pd),
            AnyExpressionNode::MethodCall(node) => self.visit_method_call(node, pd),
            AnyExpressionNode::Constructor(node) => self.visit_constructor(node, pd),
        }
    }
}
