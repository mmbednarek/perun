use crate::token::Keyword;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, AnyType};
use inkwell::context::Context;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType<T> {
    pub args: Vec<T>,
    pub ret_type: T,
}

type FuncTypeBox<T> = Box<FuncType<T>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType<T> {
    pub fields: Vec<T>,
}

type StructTypeBox<T> = Box<StructType<T>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    RawPtr,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
    Bool,
    Struct(StructTypeBox<Type>),
    Alias(String),
    Function(FuncTypeBox<Type>),
}

fn struct_to_llvm_type<'ctx>(ctx: &'ctx Context, struct_type: &StructType<Type>) -> Option<inkwell::types::StructType<'ctx>> {
    let mut basic_types: Vec<BasicTypeEnum> = Vec::new();
    for field in &struct_type.fields {
        basic_types.push(field.to_llvm_basic_type(ctx)?);
    }

    Some(ctx.struct_type(&basic_types, false))
}

impl Type {
    pub fn from_keyword(kw: &Keyword) -> Option<Type> {
        match kw {
            Keyword::Void => Some(Type::Void),
            Keyword::RawPtr => Some(Type::RawPtr),
            Keyword::Int8 => Some(Type::Int8),
            Keyword::Int16 => Some(Type::Int16),
            Keyword::Int32 => Some(Type::Int32),
            Keyword::Int64 => Some(Type::Int64),
            Keyword::Float32 => Some(Type::Float32),
            Keyword::Float64 => Some(Type::Float64),
            Keyword::Bool => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn to_llvm_basic_type<'ctx>(&self, ctx: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Type::RawPtr => Some(BasicTypeEnum::PointerType(ctx.ptr_type(inkwell::AddressSpace::from(0)))),
            Type::Int8 => Some(BasicTypeEnum::IntType(ctx.i8_type())),
            Type::Int16 => Some(BasicTypeEnum::IntType(ctx.i16_type())),
            Type::Int32 => Some(BasicTypeEnum::IntType(ctx.i32_type())),
            Type::Int64 => Some(BasicTypeEnum::IntType(ctx.i64_type())),
            Type::Float32 => Some(BasicTypeEnum::FloatType(ctx.f32_type())),
            Type::Float64 => Some(BasicTypeEnum::FloatType(ctx.f64_type())),
            Type::Bool => Some(BasicTypeEnum::IntType(ctx.bool_type())),
            Type::Struct(struct_type) => Some(BasicTypeEnum::StructType(struct_to_llvm_type(ctx, struct_type.as_ref())?)),
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
            Type::Int8 => true,
            Type::Int16 => true,
            Type::Int32 => true,
            Type::Int64 => true,
            Type::Bool => true,
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
            Type::Float32 => true,
            Type::Float64 => true,
            _ => false,
        }
    }

    pub fn byte_count(&self) -> Option<i32> {
        match self {
            Type::Int8 => Some(1),
            Type::Int16 => Some(2),
            Type::Int32 => Some(4),
            Type::Int64 => Some(8),
            Type::Float32 => Some(4),
            Type::Float64 => Some(8),
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

#[macro_export]
macro_rules! visit_type {
    ($loc:expr, $ctx:expr, $x:expr, $y:ident, $z:expr) => {
        match wrap_option($loc, $x.to_llvm_type($ctx), "failed to map llvm type")? {
            AnyTypeEnum::PointerType($y) => wrap_err($loc, $z),
            AnyTypeEnum::IntType($y) => wrap_err($loc, $z),
            AnyTypeEnum::FloatType($y) => wrap_err($loc, $z),
            AnyTypeEnum::StructType($y) => wrap_err($loc, $z),
            _ => { Err(crate::error::CompilerError{location: $loc, message: format!("failed to map to llvm type: {:?}", *$x)}) },
        }
    };
}

#[macro_export]
macro_rules! visit_any_type {
    ($loc:expr, $ctx:expr, $x:expr, $y:ident, $z:expr) => {
        match wrap_option($loc, $x.to_llvm_type($ctx), "failed to map llvm type")? {
            AnyTypeEnum::PointerType($y) => wrap_err($loc, $z),
            AnyTypeEnum::IntType($y) => wrap_err($loc, $z),
            AnyTypeEnum::FloatType($y) => wrap_err($loc, $z),
            AnyTypeEnum::StructType($y) => wrap_err($loc, $z),
            AnyTypeEnum::VoidType($y) => wrap_err($loc, $z),
            _ => { Err(crate::error::CompilerError{location: $loc, message: format!("failed to map to llvm type: {:?}", *$x)}) },
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    RValue,
    LValue,
    None,
}
