use crate::error::{SymbolLookupError, SymbolLookupResult};
use crate::token::Location;
use crate::typing::{Identifier, Type};
use std::collections::btree_map::Range;
use std::collections::BTreeMap;

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolType {
    FunctionDef,
    ConstantDef,
    TypeDef,
    FunctionArg(usize, bool),
    StructField(usize),
    LocalVariable,
    LocalReference,
    Namespace,
}

#[derive(Debug)]
pub struct SymbolInfo {
    pub name: String,
    pub sym_type: SymbolType,
    pub data_type: Type,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolPath {
    path: String,
}

impl SymbolPath {
    pub fn empty() -> Self {
        Self { path: "".into() }
    }

    pub fn new(module_name: &str) -> Self {
        assert!(!module_name.is_empty());
        Self {
            path: module_name.to_string(),
        }
    }

    pub fn add_sub(&mut self, name: &str) {
        if !self.path.is_empty() {
            self.path.push('.');
        }
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
        Self { path: res_path }
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
        Self {
            symbols: BTreeMap::new(),
        }
    }

    pub fn add_symbol(
        &mut self,
        symbol_path: &SymbolPath,
        symbol_info: SymbolInfo,
    ) -> SymbolLookupResult<()> {
        let key = symbol_path.sub(&symbol_info.name);
        if self.symbols.contains_key(&key) {
            return Err(SymbolLookupError::AlreadyRegistered(symbol_info.name));
        }
        self.symbols.insert(key, symbol_info);
        Ok(())
    }

    pub fn find_symbol_path(
        &self,
        lookup_path: &SymbolPath,
        name: &str,
    ) -> SymbolLookupResult<SymbolPath> {
        let mut path = lookup_path.clone();

        loop {
            let subpath = path.sub(name);
            if self.symbols.contains_key(&subpath) {
                return Ok(subpath);
            }
            if path.is_empty() {
                break;
            }
            path.truncate_to_parent();
        }

        Err(SymbolLookupError::NoSymbolFound(name.to_string()))
    }

    pub fn find_identifier_path(
        &self,
        lookup_path: &SymbolPath,
        identifier: &Identifier,
    ) -> SymbolLookupResult<SymbolPath> {
        let identifier_path = self.get_identifier_path(lookup_path, identifier)?;
        self.find_symbol_path(&identifier_path, &identifier.value)
    }

    pub fn find_symbol_with_path(
        &self,
        lookup_path: &SymbolPath,
        name: &str,
    ) -> SymbolLookupResult<(&SymbolInfo, SymbolPath)> {
        let mut path = lookup_path.clone();

        loop {
            let sym = self.symbols.get(&path.sub(name));
            if let Some(symbol) = sym {
                return Ok((symbol, path));
            }

            if path.is_empty() {
                break;
            }
            path.truncate_to_parent();
        }

        Err(SymbolLookupError::NoSymbolFound(name.to_string()))
    }

    pub fn find_symbol(
        &self,
        lookup_path: &SymbolPath,
        name: &str,
    ) -> SymbolLookupResult<&SymbolInfo> {
        let (info, _) = self.find_symbol_with_path(lookup_path, name)?;
        Ok(info)
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
                let symbol = self.find_identifier(path, &alias)?;
                Ok(symbol.data_type.clone())
            }
            Type::StaticArray {
                element_type,
                count,
            } => {
                let resolved_subtype =
                    self.resolve_type_alias(path, element_type.as_ref().clone())?;
                Ok(Type::StaticArray {
                    element_type: Box::new(resolved_subtype),
                    count,
                })
            }
            tp => Ok(tp),
        }
    }

    pub fn get_identifier_path(
        &self,
        path: &SymbolPath,
        identifier: &Identifier,
    ) -> SymbolLookupResult<SymbolPath> {
        Ok(if let Some(ns) = &identifier.namespace {
            let (namespace_sym, sym_path) = self.find_symbol_with_path(path, ns)?;
            if namespace_sym.sym_type != SymbolType::Namespace
                && namespace_sym.sym_type != SymbolType::TypeDef
            {
                return Err(SymbolLookupError::NotANamespace);
            }
            sym_path.sub(namespace_sym.name.as_ref())
        } else {
            self.find_symbol_path(path, identifier.value.as_str())?
        })
    }

    pub fn find_identifier(
        &self,
        path: &SymbolPath,
        identifier: &Identifier,
    ) -> SymbolLookupResult<&SymbolInfo> {
        let alias_path = self.get_identifier_path(path, identifier)?;
        Ok(self.find_symbol(&alias_path, &identifier.value)?)
    }
}
