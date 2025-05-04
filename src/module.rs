use crate::ast::{
    AliasNode, AnyExpressionNode, ConstDeclNode, EnumNode, ExpressionBox, FunctionArg,
    FunctionLinkage, FunctionNode, StructField, StructNode,
};
use crate::lexer::Lexer;
use crate::module_api::{load_module, ModuleCore};
use crate::parser::Parser;
use crate::token::{Location, OperatorType, Token, TokenType};
use crate::token_reader::TokenReader;
use crate::typing::{DataSize, Type};

pub struct Module {
    pub name: String,
    pub functions: Vec<FunctionNode>,
    pub constants: Vec<ConstDeclNode>,
    pub structs: Vec<StructNode>,
    pub enums: Vec<EnumNode>,
    pub aliases: Vec<AliasNode>,
}

fn expression_to_string(expr: &AnyExpressionNode) -> String {
    match expr {
        AnyExpressionNode::Number(num) => num.number.to_string(),
        AnyExpressionNode::String(str) => str.value.clone(),
        _ => "0".into(),
    }
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
            name: module_core.name.clone(),
            functions: Vec::new(),
            constants: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            aliases: Vec::new(),
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
                is_public: true,
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
                is_public: true,
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
                is_public: true,
            });
        }

        for enum_value in &module_core.enums {
            module.enums.push(EnumNode {
                location,
                name: enum_value.name.clone(),
                enumerations: enum_value.enumerations.clone(),
                is_public: true,
            })
        }

        for alias_value in &module_core.aliases {
            module.aliases.push(AliasNode {
                location,
                is_public: true,
                name: alias_value.name.clone(),
                aliased_type: Type::from_string(
                    Some(module_core.name.clone()),
                    alias_value.aliased_type.as_ref(),
                ),
            })
        }

        Some(module)
    }

    pub fn to_api(&self) -> ModuleCore {
        let mut module = ModuleCore {
            name: self.name.clone(),
            functions: vec![],
            constants: vec![],
            structs: vec![],
            enums: vec![],
            aliases: vec![],
        };

        for func in &self.functions {
            let mut arguments = Vec::<crate::module_api::FunctionArg>::new();
            for arg in &func.params {
                arguments.push(crate::module_api::FunctionArg {
                    name: arg.name.clone(),
                    is_ref: arg.is_ref,
                    arg_type: arg.arg_type.to_string(),
                });
            }

            module.functions.push(crate::module_api::Function {
                name: func.name.clone(),
                receiver: func.self_type.clone().map(|t| t.to_string()),
                return_type: func.ret_type.to_string(),
                args: arguments,
            })
        }

        for constant in &self.constants {
            module.constants.push(crate::module_api::Constant {
                name: constant.name.clone(),
                const_type: constant
                    .const_type
                    .clone()
                    .unwrap_or(Type::Integer(true, DataSize::Bits32))
                    .to_string(),
                value: expression_to_string(&constant.value),
            })
        }

        for structure in &self.structs {
            let mut struct_fields = Vec::<crate::module_api::StructArg>::new();
            for field in &structure.fields {
                struct_fields.push(crate::module_api::StructArg {
                    name: field.name.clone(),
                    arg_type: field.field_type.to_string(),
                });
            }

            module.structs.push(crate::module_api::Struct {
                name: structure.name.clone(),
                arguments: struct_fields,
            });
        }

        for enum_node in &self.enums {
            module.enums.push(crate::module_api::Enum {
                name: enum_node.name.clone(),
                enumerations: enum_node.enumerations.clone(),
            });
        }

        for alias_node in &self.aliases {
            module.aliases.push(crate::module_api::Alias {
                name: alias_node.name.clone(),
                aliased_type: alias_node.aliased_type.to_string(),
            })
        }

        module
    }
}
