use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{PointerValue, BasicValueEnum, BasicValue, IntValue};
use inkwell::OptimizationLevel;
use crate::token::Location;
use crate::symbols::{SymbolTable, SymbolInfo, SymbolPath};
use crate::address_table::AddressTable;
use crate::typing::Type;
use crate::error::{CompilerResult, wrap_option, wrap_err, err_with_location};

type MainFunc = unsafe extern "C" fn() -> i32;

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

    pub fn run(&mut self) -> i32 {
        unsafe{ self.exe.get_function::<MainFunc>("main").unwrap().call() }
    }

    pub fn load_var(&self, location: Location, var_type: &Type, ptr: &PointerValue<'ctx>, name: &str) -> CompilerResult<BasicValueEnum<'ctx>> {
        visit_type!(location, self.context, var_type, value, self.builder.build_load(value, *ptr, name))
    }

    pub fn alloc_var(&self, location: Location, var_type: &Type, name: &str) -> CompilerResult<PointerValue<'ctx>> {
        visit_type!(location, self.context, var_type, value, self.builder.build_alloca(value, name))
    }

    pub fn build_get_element_ptr(&self, location: Location, ptr_type: &Type, ptr: PointerValue<'ctx>, index: IntValue<'ctx>, name: &str) -> CompilerResult<PointerValue<'ctx>> {
        visit_type!(location, self.context, ptr_type, value, unsafe{ self.builder.build_gep(value, ptr, &[index], name) })
    }

    pub fn build_sext(&self, location: Location, target_type: &Type, int_value: IntValue<'ctx>, name: &str) -> CompilerResult<IntValue<'ctx>> {
        match wrap_option(location, target_type.to_llvm_type(self.context), "failed to map llvm type")? {
            AnyTypeEnum::IntType(int_type) => wrap_err(location, self.builder.build_int_s_extend(int_value, int_type, name)),
            _ => { Err(crate::error::CompilerError{location, message: format!("expected an int type, got: {:?}", target_type)}) },
        }
    }

    pub fn build_trunc(&self, location: Location, target_type: &Type, int_value: IntValue<'ctx>, name: &str) -> CompilerResult<IntValue<'ctx>> {
        match wrap_option(location, target_type.to_llvm_type(self.context), "failed to map llvm type")? {
            AnyTypeEnum::IntType(int_type) => wrap_err(location, self.builder.build_int_truncate(int_value, int_type, name)),
            _ => { Err(crate::error::CompilerError{location, message: format!("expected an int type, got: {:?}", target_type)}) },
        }
    }

    pub fn find_symbol_with_addr(&self, location: Location, path: &SymbolPath, name: &str) -> CompilerResult<(&SymbolInfo, &PointerValue<'ctx>)> {
        let sym = err_with_location(location, self.symtable.find_symbol(path, name))?;
        let ptr = err_with_location(location, self.addrtable.find_symbol(path, name))?;
        Ok((sym, ptr))
    }

    pub fn null_ptr(&self) -> Box<dyn BasicValue<'ctx> + 'ctx>{
        Box::new(self.context.ptr_type(0.into()).const_null())
    }
}
