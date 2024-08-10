use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{PointerValue, BasicValueEnum};
use inkwell::OptimizationLevel;
use crate::token::Location;
use crate::symbols::{SymbolTable, SymbolInfo, SymbolPath};
use crate::address_table::AddressTable;
use crate::typing::Type;
use crate::error::{CompilerResult, CompilerError, wrap_option, wrap_err};

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

    pub fn load_var(&self, location: Location, var_type: &Type, ptr: &PointerValue<'ctx>, name: &str) -> CompilerResult<BasicValueEnum<'ctx>> {
        let llvm_type = wrap_option(location, var_type.to_llvm_type(self.context), "failed to map llvm type A")?;

        match llvm_type {
            AnyTypeEnum::PointerType(pt) => wrap_err(location, self.builder.build_load(pt, *ptr, name)),
            AnyTypeEnum::IntType(it) => wrap_err(location, self.builder.build_load(it, *ptr, name)),
            AnyTypeEnum::FloatType(ft) => wrap_err(location, self.builder.build_load(ft, *ptr, name)),
            _ => { Err(CompilerError{location, message: format!("failed to map to llvm type: {:?}", *var_type)}) },
        }
    }

    pub fn alloc_var(&self, location: Location, var_type: &Type, name: &str) -> CompilerResult<PointerValue<'ctx>> {
        let llvm_type = wrap_option(location, var_type.to_llvm_type(self.context), "failed to map llvm type C")?;

        match llvm_type {
            AnyTypeEnum::PointerType(pt) => wrap_err(location, self.builder.build_alloca(pt, name)),
            AnyTypeEnum::IntType(it) => wrap_err(location, self.builder.build_alloca(it, name)),
            AnyTypeEnum::FloatType(ft) => wrap_err(location, self.builder.build_alloca(ft, name)),
            _ => { Err(CompilerError{location, message: "failed to map to llvm type D".to_string()}) },
        }
    }

    pub fn find_symbol_with_addr(&self, location: Location, path: &SymbolPath, name: &str) -> CompilerResult<(&SymbolInfo, &PointerValue<'ctx>)> {
        let msg = format!("unable to find symbol {}", name);
        let sym = wrap_option(location, self.symtable.find_symbol(path, name), msg.as_ref())?;
        let ptr = wrap_option(location, self.addrtable.find_symbol(path, name), msg.as_ref())?;
        Ok((sym, ptr))
    }
}
