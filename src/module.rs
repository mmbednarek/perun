use crate::module_api::{load_module, ModuleCore};
use crate::typing::{FuncType, FuncTypeArg, Type};
use std::collections::BTreeMap;

pub struct Module {
    pub functions: BTreeMap<String, FuncType<Type>>,
}

impl Module {
    pub fn new(path: &str) -> Option<Self> {
        Some(Self::from_api(&load_module(path)?))
    }
    pub fn from_api(module_core: &ModuleCore) -> Self {
        let mut module = Self {
            functions: BTreeMap::new(),
        };

        for func in &module_core.functions {
            let mut dst_args = Vec::<FuncTypeArg<Type>>::new();
            for arg in &func.args {
                dst_args.push(FuncTypeArg::<Type> {
                    is_ref: arg.is_ref,
                    arg_type: Type::from_string(arg.arg_type.as_ref()),
                });
            }

            let return_type = Type::from_string(func.return_type.as_ref());

            module.functions.insert(
                func.name.clone(),
                FuncType::<Type> {
                    args: dst_args,
                    ret_type: return_type,
                },
            );
        }

        module
    }
}
