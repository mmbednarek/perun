use std::collections::btree_map::Range;
use std::collections::BTreeMap;
use crate::typing::Type;
use crate::token::Location;
use crate::error::{SymbolLookupError, SymbolLookupResult};

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolType {
    FunctionDef,
    ConstantDef,
    TypeDef,
    FunctionArg(usize, bool),
    StructField(usize),
    LocalVariable,
    LocalReference,
}

#[derive(Debug)]
pub struct SymbolInfo {
    pub name: String,
    pub sym_type: SymbolType,
    pub data_type: Type,
    pub location: Location,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolPath {
    path: String,
}

impl SymbolPath {
    pub fn new(module_name: &str) -> Self {
        Self{path: module_name.to_string()}
    }

    pub fn add_sub(&mut self, name: &str) {
        self.path.push('.');
        self.path.push_str(name);
    }

    pub fn sub(&self, name: &str) -> Self {
        let mut result = self.clone();
        result.add_sub(name);
        result
    }

    pub fn truncate_to_parent(&mut self) {
        let last_dot = self.path.rfind('.');
        match last_dot {
            Some(pos) => self.path.truncate(pos),
            None => self.path.clear(),
        }
    }

    pub fn parent(&self) -> Self {
        let mut result = self.clone();
        result.truncate_to_parent();
        result
    }

    pub fn as_range_end(&self) -> Self {
        let mut res_path = self.path.clone();
        res_path.push('/');
        Self{path: res_path}
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }
}

impl std::fmt::Display for SymbolPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path)
    }
}

pub struct SymbolTable {
    symbols: BTreeMap<SymbolPath, SymbolInfo>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self{symbols: BTreeMap::new()}
    }

    pub fn add_symbol(&mut self, sympath: &SymbolPath, syminfo: SymbolInfo) -> SymbolLookupResult<()> {
        let key = sympath.sub(&syminfo.name);
        if self.symbols.contains_key(&key) {
            return Err(SymbolLookupError::AlreadyRegistered(syminfo.name));
        }
        self.symbols.insert(key, syminfo);
        Ok(())
    }

    pub fn find_symbol_path(&self, lookup_path: &SymbolPath, name: &str) -> SymbolLookupResult<SymbolPath> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let subpath = path.sub(name);
            if self.symbols.contains_key(&subpath) {
                return Ok(subpath);
            }
            path.truncate_to_parent();
        }

        Err(SymbolLookupError::NoSymbolFound(name.to_string()))
    }

    pub fn find_symbol(&self, lookup_path: &SymbolPath, name: &str) -> SymbolLookupResult<&SymbolInfo> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let sym = self.symbols.get(&path.sub(name));
            if let Some(symbol) = sym {
                return Ok(symbol);
            }
            path.truncate_to_parent();
        }

        Err(SymbolLookupError::NoSymbolFound(name.to_string()))
    }

    pub fn find_by_path(&self, path: &SymbolPath) -> Option<&SymbolInfo> {
        let mut current_path = path.clone();
        while !current_path.is_empty() {
            let sym = self.symbols.get(&current_path);
            if sym.is_some() {
                return sym;
            }
            current_path.truncate_to_parent();
        }
        None
    }

    pub fn print_symbols(&self) {
        for (key, value) in &self.symbols {
            println!("SYMBOL {}: {:?}", key, value);
        }
    }

    pub fn iterate_path(&self, path: &SymbolPath) -> Range<SymbolPath, SymbolInfo> {
        self.symbols.range(path.clone()..path.as_range_end())
    }

    pub fn resolve_type_alias(&self, path: &SymbolPath, in_type: Type) -> SymbolLookupResult<Type> {
        match in_type {
            Type::Alias(alias) => {
                let symbol = self.find_symbol(path, &alias)?;
                Ok(symbol.data_type.clone())
            },
            tp => Ok(tp),
        }
    }
}
