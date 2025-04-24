use std::path::Path;

use crate::error::{CompilerResult, CompilerResultErrorMapper, CompilerResultErrorMapperWithDesc};
use crate::ir_value_storage::IRValueStorage;
use crate::symbols::{SymbolInfo, SymbolPath, SymbolTable};
use crate::token::Location;
use crate::typing::Type;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::targets::{FileType, RelocMode, Target, TargetMachine, TargetMachineOptions};
use inkwell::types::AnyTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue};
use inkwell::OptimizationLevel;

type MainFunc = unsafe extern "C" fn() -> i32;

pub trait BasicValueExtension<'ctx> {
    fn to_int(&self, location: Location) -> CompilerResult<IntValue<'ctx>>;
    fn to_float(&self, location: Location) -> CompilerResult<FloatValue<'ctx>>;
    fn to_ptr(&self, location: Location) -> CompilerResult<PointerValue<'ctx>>;
    #[allow(dead_code)]
    fn is_ptr(&self) -> bool;
}

impl<'ctx> BasicValueExtension<'ctx> for dyn BasicValue<'ctx> + '_ {
    fn to_int(&self, location: Location) -> CompilerResult<IntValue<'ctx>> {
        if let BasicValueEnum::IntValue(int_val) = self.as_basic_value_enum() {
            Ok(int_val)
        } else {
            compiler_err!(
                location,
                "invalid type, value {} is not int",
                self.print_to_string()
            );
        }
    }

    fn to_float(&self, location: Location) -> CompilerResult<FloatValue<'ctx>> {
        if let BasicValueEnum::FloatValue(float_val) = self.as_basic_value_enum() {
            Ok(float_val)
        } else {
            compiler_err!(
                location,
                "invalid type, value {} is not float",
                self.print_to_string()
            );
        }
    }

    fn to_ptr(&self, location: Location) -> CompilerResult<PointerValue<'ctx>> {
        if let BasicValueEnum::PointerValue(ptr_val) = self.as_basic_value_enum() {
            Ok(ptr_val)
        } else {
            compiler_err!(
                location,
                "invalid type, value {} is not ptr",
                self.print_to_string()
            );
        }
    }

    fn is_ptr(&self) -> bool {
        match self.as_basic_value_enum() {
            BasicValueEnum::PointerValue(_) => true,
            _ => false,
        }
    }
}

impl<'ctx> BasicValueExtension<'ctx> for BasicValueEnum<'ctx> {
    fn to_int(&self, location: Location) -> CompilerResult<IntValue<'ctx>> {
        (self as &dyn BasicValue<'ctx>).to_int(location)
    }

    fn to_float(&self, location: Location) -> CompilerResult<FloatValue<'ctx>> {
        (self as &dyn BasicValue<'ctx>).to_float(location)
    }

    fn to_ptr(&self, location: Location) -> CompilerResult<PointerValue<'ctx>> {
        (self as &dyn BasicValue<'ctx>).to_ptr(location)
    }

    fn is_ptr(&self) -> bool {
        match self {
            BasicValueEnum::PointerValue(_) => true,
            _ => false,
        }
    }
}

pub struct IRBuildContext<'ctx, 'st> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub symbol_table: &'st SymbolTable,
    pub ir_value_storage: IRValueStorage<'ctx>,
    pub target_machine: TargetMachine,
}

impl<'ctx, 'st> IRBuildContext<'ctx, 'st> {
    pub fn new(context: &'ctx Context, symbol_table: &'st SymbolTable) -> Self {
        let module = context.create_module("output");
        let builder = context.create_builder();
        let execution_engine: ExecutionEngine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let target_config = TargetMachineOptions::default()
            .set_cpu("generic")
            .set_features("")
            .set_reloc_mode(RelocMode::PIC);

        Self {
            context,
            module,
            builder,
            execution_engine,
            symbol_table,
            ir_value_storage: IRValueStorage::new(),
            target_machine: target
                .create_target_machine_from_options(&triple, target_config)
                .unwrap(),
        }
    }

    pub fn print_module(&self) {
        self.module.print_to_stderr();
    }

    pub fn run(&mut self) -> i32 {
        unsafe {
            self.execution_engine
                .get_function::<MainFunc>("main")
                .unwrap()
                .call()
        }
    }

    pub fn compile(&self, file: FileType, path: &Path) {
        self.target_machine
            .write_to_file(&self.module, file, path)
            .unwrap();
    }

    pub fn build_sext(
        &self,
        location: Location,
        target_type: &Type,
        int_value: IntValue<'ctx>,
        name: &str,
    ) -> CompilerResult<IntValue<'ctx>> {
        match target_type
            .to_llvm_type(self.context)
            .to_comp_res_with_desc(location, "failed to map llvm type")?
        {
            AnyTypeEnum::IntType(int_type) => self
                .builder
                .build_int_s_extend(int_value, int_type, name)
                .to_comp_res(location),
            _ => Err(crate::error::CompilerError {
                location,
                message: format!("expected an int type, got: {:?}", target_type),
            }),
        }
    }

    pub fn build_trunc(
        &self,
        location: Location,
        target_type: &Type,
        int_value: IntValue<'ctx>,
        name: &str,
    ) -> CompilerResult<IntValue<'ctx>> {
        match target_type
            .to_llvm_type(self.context)
            .to_comp_res_with_desc(location, "failed to map llvm type")?
        {
            AnyTypeEnum::IntType(int_type) => self
                .builder
                .build_int_truncate(int_value, int_type, name)
                .to_comp_res(location),
            _ => Err(crate::error::CompilerError {
                location,
                message: format!("expected an int type, got: {:?}", target_type),
            }),
        }
    }

    pub fn find_symbol_with_addr(
        &self,
        location: Location,
        path: &SymbolPath,
        name: &str,
    ) -> CompilerResult<(&SymbolInfo, &PointerValue<'ctx>)> {
        let sym = self
            .symbol_table
            .find_symbol(path, name)
            .to_comp_res(location)?;
        let ptr = self
            .ir_value_storage
            .find_symbol(path, name)
            .to_comp_res(location)?;
        Ok((sym, ptr))
    }

    pub fn null_ptr(&self) -> Box<dyn BasicValue<'ctx> + 'ctx> {
        Box::new(self.context.ptr_type(0.into()).const_null())
    }
}
