use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{PointerValue, BasicValueEnum, BasicValue, IntValue, FunctionValue};
use inkwell::targets::{TargetMachine, Target, RelocMode, CodeModel, FileType, TargetMachineOptions};
use inkwell::basic_block::BasicBlock;
use inkwell::{OptimizationLevel, IntPredicate};
use crate::ast::ExpressionNode;
use crate::token::Location;
use crate::symbols::{SymbolTable, SymbolInfo, SymbolPath};
use crate::address_table::AddressTable;
use crate::typing::{Type, ValueType};
use crate::error::{err_with_location, wrap_err, wrap_option, CompilerResult};

type MainFunc = unsafe extern "C" fn() -> i32;

pub fn basic_value_to_int<'ctx>(location: Location, value: &dyn BasicValue<'ctx>) -> CompilerResult<IntValue<'ctx>> {
    if let BasicValueEnum::IntValue(int_val) = value.as_basic_value_enum() {
        return Ok(int_val);
    } else {
        compiler_err!(location, "invalid type, value {} is not int", value.print_to_string());
    }
}

pub fn basic_value_to_ptr<'ctx>(location: Location, value: &dyn BasicValue<'ctx>) -> CompilerResult<PointerValue<'ctx>> {
    if let BasicValueEnum::PointerValue(ptr_val) = value.as_basic_value_enum() {
        return Ok(ptr_val);
    } else {
        compiler_err!(location, "invalid type, value {} is not ptr", value.print_to_string());
    }
}

pub struct IlGenerator<'ctx, 'st> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub exe: ExecutionEngine<'ctx>,
    pub symtable: &'st SymbolTable,
    pub addrtable: AddressTable<'ctx>,
    pub machine: TargetMachine,
}

impl<'ctx, 'st> IlGenerator<'ctx, 'st> {
    pub fn new(context: &'ctx Context, symtable: &'st SymbolTable) -> Self {
        let module = context.create_module("output");
        let builder = context.create_builder();
        let exe: ExecutionEngine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let target_config = TargetMachineOptions::default().set_cpu("generic").set_features("").set_reloc_mode(RelocMode::PIC);

        Self{
            context,
            module,
            builder,
            exe,
            symtable,
            addrtable: AddressTable::new(),
            machine: target.create_target_machine_from_options(&triple, target_config).unwrap(),
        }
    }

    pub fn print_module(&self) {
        self.module.print_to_stderr();
    }

    pub fn run(&mut self) -> i32 {
        unsafe{ self.exe.get_function::<MainFunc>("main").unwrap().call() }
    }

    pub fn compile(&self, file: FileType, path: &Path) {
        self.machine.write_to_file(&self.module, file, path).unwrap();
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

    pub fn build_cast(&self, location: Location, source_type: &Type, target_type: &Type, value: Box<dyn BasicValue<'ctx> + 'ctx>) -> CompilerResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if *source_type == *target_type {
            return Ok(value);
        }

        if !source_type.is_int_type() {
            compiler_err!(location, "source cast must be an int");
        }

        let int_value = basic_value_to_int(location, value.as_ref())?;

        if target_type.is_bool_type() {
            Ok(Box::new(wrap_err(location, self.builder.build_int_compare(IntPredicate::NE, int_value, self.context.i32_type().const_int(0, true), "tobool"))?))
        } else if target_type.is_int_type() {

            if target_type.byte_count() > source_type.byte_count() {
                Ok(Box::new(self.build_sext(location, target_type, int_value, "intsext")?))
            } else {
                Ok(Box::new(self.build_trunc(location, target_type, int_value, "inttrunc")?))
            }
        } else {
            compiler_err!(location, "unsuported cast target type");
        }
    }
}
