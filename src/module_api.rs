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
    pub return_type: String,
    pub args: Vec<FunctionArg>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ModuleCore {
    pub name: String,
    pub functions: Vec<Function>,
}

pub fn load_module(path: &str) -> Option<ModuleCore> {
    let file = File::open(path).ok()?;
    Some(serde_json::from_reader(file).ok()?)
}
