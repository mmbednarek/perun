use crate::ast::{
    ConstDeclNode, ExpressionBox, FunctionArg, FunctionLinkage, FunctionNode, StructField,
    StructNode,
};
use crate::lexer::Lexer;
use crate::module_api::{load_module, ModuleCore};
use crate::parser::Parser;
use crate::token::{Location, OperatorType, Token, TokenType};
use crate::token_reader::TokenReader;
use crate::typing::Type;

pub struct Module {
    pub functions: Vec<FunctionNode>,
    pub constants: Vec<ConstDeclNode>,
    pub structs: Vec<StructNode>,
}

fn parse_immediate_value(value: &str) -> Option<ExpressionBox> {
    let mut lexer = Lexer::new(Box::new(value.as_bytes()));
    lexer.read_tokens();
    let mut tokens = lexer.tokens().clone();
    tokens.push(Token {
        token_type: TokenType::Operator(OperatorType::Semicolon),
        location: Location { line: 0, column: 0 },
    });
    let mut reader = TokenReader::new(tokens.as_slice());
    let mut parser = Parser::new(&mut reader);
    parser.parse_expression(OperatorType::Semicolon).ok()
}

impl Module {
    pub fn new(path: &str, location: Location) -> Option<Self> {
        Self::from_api(&load_module(path)?, location)
    }
    pub fn from_api(module_core: &ModuleCore, location: Location) -> Option<Self> {
        let mut module = Self {
            functions: Vec::new(),
            constants: Vec::new(),
            structs: Vec::new(),
        };

        for constant in &module_core.constants {
            module.constants.push(ConstDeclNode {
                location,
                name: constant.name.clone(),
                const_type: Some(Type::from_string(
                    Some(module_core.name.clone()),
                    constant.const_type.as_ref(),
                )),
                value: parse_immediate_value(constant.value.as_str())?,
            });
        }

        for func in &module_core.functions {
            let mut params = Vec::<FunctionArg>::new();
            for arg in &func.args {
                params.push(FunctionArg {
                    location,
                    is_ref: arg.is_ref,
                    arg_type: Type::from_string(
                        Some(module_core.name.clone()),
                        arg.arg_type.as_ref(),
                    ),
                    name: arg.name.clone(),
                });
            }

            module.functions.push(FunctionNode {
                location,
                self_type: if let Some(rec) = &func.receiver {
                    Some(Type::from_string(
                        Some(module_core.name.clone()),
                        rec.as_ref(),
                    ))
                } else {
                    None
                },
                name: func.name.clone(),
                params,
                ret_type: Type::from_string(
                    Some(module_core.name.clone()),
                    func.return_type.as_ref(),
                ),
                linkage: FunctionLinkage::Standard,
                scope: None,
            });
        }

        for struct_value in &module_core.structs {
            let mut fields = Vec::<StructField>::new();
            for src_field in &struct_value.arguments {
                fields.push(StructField {
                    location,
                    name: src_field.name.clone(),
                    field_type: Type::from_string(
                        Some(module_core.name.clone()),
                        src_field.arg_type.as_ref(),
                    ),
                });
            }

            module.structs.push(StructNode {
                location,
                name: struct_value.name.clone(),
                fields,
            });
        }

        Some(module)
    }
}
