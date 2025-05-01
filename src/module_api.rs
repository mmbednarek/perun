use serde::{Deserialize, Serialize};
use std::fs::File;

#[derive(Serialize, Deserialize, Debug)]
pub struct FunctionArg {
    pub name: String,
    pub is_ref: bool,
    pub arg_type: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Function {
    pub name: String,
    pub receiver: Option<String>,
    pub return_type: String,
    pub args: Vec<FunctionArg>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Constant {
    pub name: String,
    pub const_type: String,
    pub value: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StructArg {
    pub name: String,
    pub arg_type: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Struct {
    pub name: String,
    pub arguments: Vec<StructArg>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Enum {
    pub name: String,
    pub enumerations: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ModuleCore {
    pub name: String,
    pub functions: Vec<Function>,
    pub constants: Vec<Constant>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
}

pub fn load_module(path: &str) -> Option<ModuleCore> {
    let file = File::open(path).ok()?;
    Some(serde_json::from_reader(file).ok()?)
}
