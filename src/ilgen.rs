use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use crate::symbols::SymbolTable;
use crate::address_table::AddressTable;

type FibFunc = unsafe extern "C" fn(u64) -> u64;

pub struct IlGenerator<'ctx, 'st> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub exe: ExecutionEngine<'ctx>,
    pub symtable: &'st SymbolTable,
    pub addrtable: AddressTable<'ctx>
}

impl<'ctx, 'st> IlGenerator<'ctx, 'st> {
    pub fn new(context: &'ctx Context, symtable: &'st SymbolTable) -> Self {
        let module = context.create_module("output");
        let builder = context.create_builder();
        let exe = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

        Self{
            context,
            module,
            builder,
            exe,
            symtable,
            addrtable: AddressTable::new(),
        }
    }

    pub fn print_module(&self) {
        self.module.print_to_stderr();
    }

    pub fn run(&mut self, n: u64) -> u64 {
        unsafe{ self.exe.get_function::<FibFunc>("fib").unwrap().call(n) }
    }

}
