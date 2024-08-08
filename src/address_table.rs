use std::collections::BTreeMap;
use crate::symbols::SymbolPath;
use inkwell::values::PointerValue;

pub struct AddressTable<'ctx> {
    pointers: BTreeMap<SymbolPath, PointerValue<'ctx>>,
}

impl<'ctx> AddressTable<'ctx> {
    pub fn new() -> Self {
        Self{pointers: BTreeMap::new()}
    }

    pub fn register_ptr(&mut self, path: SymbolPath, ptr: PointerValue<'ctx>) {
        self.pointers.insert(path, ptr);
    }

    pub fn find_symbol(&self, lookup_path: &SymbolPath, name: &str) -> Option<&PointerValue<'ctx>> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let sym = self.pointers.get(&path.sub(name));
            if sym.is_some() {
                return sym;
            }
            path.truncate_to_parent();
        }

        None
    }
}

