use crate::typing::Type;
use serde::{Deserialize, Serialize};
use std::fs::File;

#[derive(Serialize, Deserialize, Debug)]
pub struct FunctionArg {
    pub name: String,
    pub is_ref: bool,
    pub arg_type: Type,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Function {
    pub name: String,
    pub receiver: Option<Type>,
    pub return_type: Type,
    pub args: Vec<FunctionArg>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Constant {
    pub name: String,
    pub const_type: Type,
    pub value: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StructField {
    pub name: String,
    pub field_type: Type,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Enum {
    pub name: String,
    pub enumerations: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Alias {
    pub name: String,
    pub aliased_type: Type,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ModuleCore {
    pub name: String,
    pub functions: Vec<Function>,
    pub constants: Vec<Constant>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub aliases: Vec<Alias>,
}

pub fn load_module(path: &str) -> Option<ModuleCore> {
    let file = File::open(path).ok()?;
    Some(serde_json::from_reader(file).ok()?)
}
