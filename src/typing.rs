use crate::token::Keyword;
use inkwell::context::Context;
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt::Display;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub namespace: Option<String>,
    pub value: String,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FuncTypeArg {
    pub is_ref: bool,
    pub arg_type: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub args: Vec<FuncTypeArg>,
    pub ret_type: Type,
}

pub type FuncTypeBox = Box<FuncType>;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub fields: Vec<Type>,
}

type StructTypeBox = Box<StructType>;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct EnumType {
    pub enumerations: BTreeMap<String, i64>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum DataSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

impl DataSize {
    pub fn bit_count(&self) -> u32 {
        match self {
            DataSize::Bits8 => 8,
            DataSize::Bits16 => 16,
            DataSize::Bits32 => 32,
            DataSize::Bits64 => 64,
        }
    }

    pub fn byte_count(&self) -> u32 {
        self.bit_count() / 8
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum Type {
    Void,
    RawPtr,
    TypedPtr { inner_type: Box<Type> },
    Integer { is_signed: bool, size: DataSize },
    FloatingPoint { size: DataSize },
    StaticArray { element_type: Box<Type>, count: u32 },
    Bool,
    Struct(StructTypeBox),
    Alias(Identifier),
    Function(FuncTypeBox),
    Enum(EnumType),
}

fn struct_to_llvm_type<'ctx>(
    ctx: &'ctx Context,
    struct_type: &StructType,
) -> Option<inkwell::types::StructType<'ctx>> {
    let mut basic_types: Vec<BasicTypeEnum> = Vec::new();
    for field in &struct_type.fields {
        basic_types.push(field.to_llvm_basic_type(ctx)?);
    }

    Some(ctx.struct_type(&basic_types, false))
}

impl Type {
    pub fn with_namespace(&self, ns: &str) -> Type {
        match self {
            Type::Alias(identifier) => {
                if identifier.namespace.is_none() {
                    Type::Alias(Identifier {
                        namespace: Some(ns.to_owned()),
                        value: identifier.value.to_owned(),
                    })
                } else {
                    self.clone()
                }
            }
            _ => self.clone(),
        }
    }

    pub fn from_string(namespace: Option<String>, s: &str) -> Type {
        match s {
            "void" => Type::Void,
            "i8" => Type::Integer {
                is_signed: true,
                size: DataSize::Bits8,
            },
            "i16" => Type::Integer {
                is_signed: true,
                size: DataSize::Bits16,
            },
            "i32" => Type::Integer {
                is_signed: true,
                size: DataSize::Bits32,
            },
            "i64" => Type::Integer {
                is_signed: true,
                size: DataSize::Bits64,
            },
            "u8" => Type::Integer {
                is_signed: false,
                size: DataSize::Bits8,
            },
            "u16" => Type::Integer {
                is_signed: false,
                size: DataSize::Bits16,
            },
            "u32" => Type::Integer {
                is_signed: false,
                size: DataSize::Bits32,
            },
            "u64" => Type::Integer {
                is_signed: false,
                size: DataSize::Bits64,
            },
            "f32" => Type::FloatingPoint {
                size: DataSize::Bits32,
            },
            "f64" => Type::FloatingPoint {
                size: DataSize::Bits64,
            },
            "bool" => Type::Bool,
            "rawptr" => Type::RawPtr,
            _ => Type::Alias(Identifier {
                namespace,
                value: s.into(),
            }),
        }
    }

    pub fn from_keyword(kw: &Keyword) -> Option<Type> {
        match kw {
            Keyword::Void => Some(Type::Void),
            Keyword::RawPtr => Some(Type::RawPtr),
            Keyword::Int8 => Some(Type::Integer {
                is_signed: true,
                size: DataSize::Bits8,
            }),
            Keyword::Int16 => Some(Type::Integer {
                is_signed: true,
                size: DataSize::Bits16,
            }),
            Keyword::Int32 => Some(Type::Integer {
                is_signed: true,
                size: DataSize::Bits32,
            }),
            Keyword::Int64 => Some(Type::Integer {
                is_signed: true,
                size: DataSize::Bits64,
            }),
            Keyword::UInt8 => Some(Type::Integer {
                is_signed: false,
                size: DataSize::Bits8,
            }),
            Keyword::UInt16 => Some(Type::Integer {
                is_signed: false,
                size: DataSize::Bits16,
            }),
            Keyword::UInt32 => Some(Type::Integer {
                is_signed: false,
                size: DataSize::Bits32,
            }),
            Keyword::UInt64 => Some(Type::Integer {
                is_signed: false,
                size: DataSize::Bits64,
            }),
            Keyword::Float32 => Some(Type::FloatingPoint {
                size: DataSize::Bits32,
            }),
            Keyword::Float64 => Some(Type::FloatingPoint {
                size: DataSize::Bits64,
            }),
            Keyword::Bool => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn to_llvm_basic_type<'ctx>(&self, ctx: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Type::RawPtr | Type::TypedPtr { .. } => Some(BasicTypeEnum::PointerType(
                ctx.ptr_type(inkwell::AddressSpace::from(0)),
            )),
            Type::Integer {
                is_signed: _is_signed,
                size,
            } => Some(ctx.custom_width_int_type(size.bit_count()).into()),
            Type::FloatingPoint { size } => match size {
                DataSize::Bits8 => None,
                DataSize::Bits16 => Some(ctx.f16_type().into()),
                DataSize::Bits32 => Some(ctx.f32_type().into()),
                DataSize::Bits64 => Some(ctx.f64_type().into()),
            },
            Type::StaticArray {
                element_type,
                count,
            } => Some(
                element_type
                    .to_llvm_basic_type(ctx)?
                    .array_type(*count)
                    .into(),
            ),
            Type::Bool => Some(BasicTypeEnum::IntType(ctx.bool_type())),
            Type::Struct(struct_type) => Some(BasicTypeEnum::StructType(struct_to_llvm_type(
                ctx,
                struct_type.as_ref(),
            )?)),
            Type::Enum(_) => Some(BasicTypeEnum::IntType(ctx.i32_type())),
            _ => None,
        }
    }

    pub fn to_llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Option<AnyTypeEnum<'ctx>> {
        match self {
            Type::Void => Some(AnyTypeEnum::VoidType(ctx.void_type())),
            _ => Some(self.to_llvm_basic_type(ctx)?.as_any_type_enum()),
        }
    }

    pub fn is_int_type(&self) -> bool {
        match self {
            Type::Integer { .. } => true,
            Type::Bool => true,
            _ => false,
        }
    }

    pub fn is_ptr_type(&self) -> bool {
        match self {
            Type::RawPtr => true,
            Type::TypedPtr { .. } => true,
            _ => false,
        }
    }

    pub fn is_static_array(&self) -> bool {
        match self {
            Type::StaticArray { .. } => true,
            _ => false,
        }
    }

    pub fn is_bool_type(&self) -> bool {
        match self {
            Type::Bool => true,
            _ => false,
        }
    }

    pub fn is_float_type(&self) -> bool {
        match self {
            Type::FloatingPoint { .. } => true,
            _ => false,
        }
    }

    pub fn byte_count(&self) -> Option<u32> {
        match self {
            Type::Integer { is_signed: _, size } => Some(size.byte_count()),
            Type::FloatingPoint { size } => Some(size.byte_count()),
            _ => None,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Type::Void => true,
            _ => false,
        }
    }

    pub fn wider_type(&self, other: &Type) -> Type {
        if self.is_int_type() {
            if other.is_int_type() {
                if self.byte_count().unwrap_or(0) >= other.byte_count().unwrap_or(0) {
                    self.clone()
                } else {
                    other.clone()
                }
            } else {
                other.clone()
            }
        } else {
            self.clone()
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::RawPtr => write!(f, "rawptr"),
            Type::TypedPtr { inner_type } => {
                write!(f, "*")?;
                inner_type.as_ref().fmt(f)
            }
            Type::Integer { is_signed, size } => write!(
                f,
                "{}{}",
                if *is_signed { "i" } else { "u" },
                size.bit_count()
            ),
            Type::FloatingPoint { size } => write!(f, "f{}", size.bit_count()),
            Type::StaticArray {
                element_type,
                count,
            } => {
                element_type.as_ref().fmt(f)?;
                write!(f, "[{}]", *count)
            }
            Type::Bool => write!(f, "bool"),
            Type::Struct(args) => {
                write!(f, "struct {{")?;
                for field in &args.fields {
                    write!(f, "{},", field)?;
                }
                write!(f, "}}")
            }
            Type::Alias(v) => write!(f, "{}", &v.value),
            Type::Function(func_type) => {
                write!(f, "fn (")?;
                for field in &func_type.args {
                    if field.is_ref {
                        write!(f, "ref ")?;
                    }
                    write!(f, "{},", field.arg_type)?;
                }
                write!(f, ") : {}", func_type.ret_type)
            }
            Type::Enum(enum_type) => {
                write!(f, "enum {{")?;
                for (enum_name, index) in &enum_type.enumerations {
                    write!(f, "{} = {},", *enum_name, *index)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[macro_export]
macro_rules! visit_type {
    ($loc:expr, $ctx:expr, $x:expr, $y:ident, $z:expr) => {
        match $x
            .to_llvm_type($ctx)
            .to_comp_res_with_desc($loc, "failed to map llvm type")?
        {
            AnyTypeEnum::PointerType($y) => $z,
            AnyTypeEnum::IntType($y) => $z,
            AnyTypeEnum::FloatType($y) => $z,
            AnyTypeEnum::StructType($y) => $z,
            AnyTypeEnum::ArrayType($y) => $z,
            _ => Err(crate::error::CompilerError {
                location: $loc,
                message: format!("failed to map to llvm type: {:?}", *$x),
            }),
        }
    };
}

#[macro_export]
macro_rules! visit_any_type {
    ($loc:expr, $ctx:expr, $x:expr, $y:ident, $z:expr) => {
        match $x {
            AnyTypeEnum::PointerType($y) => $z,
            AnyTypeEnum::IntType($y) => $z,
            AnyTypeEnum::FloatType($y) => $z,
            AnyTypeEnum::StructType($y) => $z,
            AnyTypeEnum::ArrayType($y) => $z,
            AnyTypeEnum::VoidType($y) => $z,
            _ => Err(crate::error::CompilerError {
                location: $loc,
                message: format!("failed to map to llvm type: {:?}", *$x),
            }),
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    RValue,
    LValue,
    None,
}
