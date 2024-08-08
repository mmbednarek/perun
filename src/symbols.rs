use std::collections::BTreeMap;

#[derive(Debug)]
pub enum SymbolType {
    FunctionArg(usize),
    LocalVariable,
}

#[derive(Debug)]
pub struct SymbolInfo {
    pub name: String,
    pub sym_type: SymbolType,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolPath {
    path: String,
}

impl SymbolPath {
    pub fn new() -> Self {
        Self{path: "root".to_string()}
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

    pub fn add_symbol(&mut self, sympath: &SymbolPath, syminfo: SymbolInfo) {
        self.symbols.insert(sympath.sub(&syminfo.name), syminfo);
    }

    pub fn find_symbol(&self, lookup_path: &SymbolPath, name: &str) -> Option<&SymbolInfo> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let sym = self.symbols.get(&path.sub(name));
            if sym.is_some() {
                return sym;
            }
            path.truncate_to_parent();
        }

        None
    }

    pub fn print_symbols(&self) {
        for (key, value) in &self.symbols {
            println!("SYMBOL {}: {:?}", key, value);
        }
    }
}
