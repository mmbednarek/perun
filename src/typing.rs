use crate::token::Keyword;
use inkwell::types::AnyTypeEnum;
use inkwell::context::Context;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType<T> {
    pub args: Vec<T>,
    pub ret_type: T,
}

type FuncTypeBox<T> = Box<FuncType<T>>;

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
    Struct(String),
    Function(FuncTypeBox<Type>),
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
            _ => None,
        }
    }

    pub fn to_llvm_type<'ctx>(&self, ctx: &'ctx Context) -> Option<AnyTypeEnum<'ctx>> {
        match self {
            Type::Void => Some(AnyTypeEnum::VoidType(ctx.void_type())),
            Type::RawPtr => Some(AnyTypeEnum::PointerType(ctx.ptr_type(inkwell::AddressSpace::from(0)))),
            Type::Int8 => Some(AnyTypeEnum::IntType(ctx.i8_type())),
            Type::Int16 => Some(AnyTypeEnum::IntType(ctx.i16_type())),
            Type::Int32 => Some(AnyTypeEnum::IntType(ctx.i32_type())),
            Type::Int64 => Some(AnyTypeEnum::IntType(ctx.i64_type())),
            Type::Float32 => Some(AnyTypeEnum::FloatType(ctx.f32_type())),
            Type::Float64 => Some(AnyTypeEnum::FloatType(ctx.f64_type())),
            _ => None,
        }
    }

    pub fn is_int_type(&self) -> bool {
        match self {
            Type::Int8 => true,
            Type::Int16 => true,
            Type::Int32 => true,
            Type::Int64 => true,
            _ => false,
        }
    }
}


