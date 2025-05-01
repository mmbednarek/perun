use crate::ast::*;
use crate::ast_passes::type_deduction_pass::{deduce_type, TypeDeductionPass};
use crate::error::{CompilerResult, CompilerResultErrorMapper, CompilerResultErrorMapperWithDesc};
use crate::ir_build_context::{BasicValueExtension, IRBuildContext};
use crate::module::Module;
use crate::symbols::{SymbolPath, SymbolType};
use crate::token::Location;
use crate::typing::{DataSize, FuncTypeBox, Type, ValueType};
use either::Either;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatMathValue, FunctionValue,
    IntMathValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

impl AssignmentBinaryOperation {
    pub fn get_stored_value<'ctx, 'st>(
        &self,
        location: Location,
        gen: &IRBuildContext<'ctx, 'st>,
        operand_type: &Type,
        lhs: PointerValue<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        match self {
            AssignmentBinaryOperation::Assign => Ok(Box::new(rhs)),
            AssignmentBinaryOperation::Math(op) => {
                let llvm_type = operand_type
                    .to_llvm_basic_type(gen.context)
                    .to_comp_res_with_desc(location, "failed to map type")?;

                let lhs_loaded = gen
                    .builder
                    .build_load(llvm_type, lhs, "inc.lhs")
                    .to_comp_res(location)?;

                op.build_instruction(location, &gen.builder, operand_type, lhs_loaded, rhs)
            }
        }
    }
}

impl ComparisonBinaryOperation {
    pub fn to_llvm_int_predicate(&self, is_signed: bool) -> IntPredicate {
        match self {
            ComparisonBinaryOperation::Equals => IntPredicate::EQ,
            ComparisonBinaryOperation::NotEquals => IntPredicate::NE,
            ComparisonBinaryOperation::Less => {
                if is_signed {
                    IntPredicate::SLT
                } else {
                    IntPredicate::ULT
                }
            }
            ComparisonBinaryOperation::LessOrEqual => {
                if is_signed {
                    IntPredicate::SLE
                } else {
                    IntPredicate::ULE
                }
            }
            ComparisonBinaryOperation::Greater => {
                if is_signed {
                    IntPredicate::SGT
                } else {
                    IntPredicate::UGT
                }
            }
            ComparisonBinaryOperation::GreaterOrEqual => {
                if is_signed {
                    IntPredicate::SGE
                } else {
                    IntPredicate::UGE
                }
            }
        }
    }

    pub fn to_llvm_float_predicate(&self) -> FloatPredicate {
        match self {
            ComparisonBinaryOperation::Equals => FloatPredicate::OEQ,
            ComparisonBinaryOperation::NotEquals => FloatPredicate::ONE,
            ComparisonBinaryOperation::Less => FloatPredicate::OLT,
            ComparisonBinaryOperation::LessOrEqual => FloatPredicate::OLE,
            ComparisonBinaryOperation::Greater => FloatPredicate::OGT,
            ComparisonBinaryOperation::GreaterOrEqual => FloatPredicate::OGE,
        }
    }
}

impl MathBinaryOperation {
    fn build_int_instruction<'ctx, T: IntMathValue<'ctx>>(
        &self,
        location: Location,
        builder: &Builder<'ctx>,
        lhs: T,
        rhs: T,
        is_signed: bool,
    ) -> CompilerResult<T> {
        match self {
            MathBinaryOperation::Add => {
                builder.build_int_add(lhs, rhs, "add").to_comp_res(location)
            }
            MathBinaryOperation::Subtract => {
                builder.build_int_sub(lhs, rhs, "sub").to_comp_res(location)
            }
            MathBinaryOperation::Multiply => {
                builder.build_int_mul(lhs, rhs, "mul").to_comp_res(location)
            }
            MathBinaryOperation::Divide => if is_signed {
                builder.build_int_signed_div(lhs, rhs, "sdiv")
            } else {
                builder.build_int_unsigned_div(lhs, rhs, "udiv")
            }
            .to_comp_res(location),
            MathBinaryOperation::Modulo => if is_signed {
                builder.build_int_signed_rem(lhs, rhs, "srem")
            } else {
                builder.build_int_unsigned_rem(lhs, rhs, "urem")
            }
            .to_comp_res(location),
        }
    }

    fn build_float_instruction<'ctx, T: FloatMathValue<'ctx>>(
        &self,
        location: Location,
        builder: &Builder<'ctx>,
        lhs: T,
        rhs: T,
    ) -> CompilerResult<T> {
        match self {
            MathBinaryOperation::Add => builder
                .build_float_add(lhs, rhs, "fadd")
                .to_comp_res(location),
            MathBinaryOperation::Subtract => builder
                .build_float_sub(lhs, rhs, "fsub")
                .to_comp_res(location),
            MathBinaryOperation::Multiply => builder
                .build_float_mul(lhs, rhs, "fmul")
                .to_comp_res(location),
            MathBinaryOperation::Divide => builder
                .build_float_div(lhs, rhs, "fdiv")
                .to_comp_res(location),
            MathBinaryOperation::Modulo => builder
                .build_float_rem(lhs, rhs, "frem")
                .to_comp_res(location),
        }
    }

    fn build_instruction<'ctx>(
        &self,
        location: Location,
        builder: &Builder<'ctx>,
        operand_type: &Type,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        match operand_type {
            Type::Integer(is_signed, _) => {
                let lhs_int = lhs.to_int(location)?;
                let rhs_int = rhs.to_int(location)?;

                Ok(Box::new(self.build_int_instruction(
                    location, builder, lhs_int, rhs_int, *is_signed,
                )?))
            }
            Type::FloatingPoint(_) => {
                let lhs_float = lhs.to_float(location)?;
                let rhs_float = rhs.to_float(location)?;

                Ok(Box::new(self.build_float_instruction(
                    location, builder, lhs_float, rhs_float,
                )?))
            }
            _ => compiler_err!(location, "invalid operand type"),
        }
    }
}

impl BinaryOperation {
    pub fn translate<'ctx, 'st>(
        &self,
        gen: &IRBuildContext<'ctx, 'st>,
        location: &Location,
        operand_type: &Type,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        match self {
            BinaryOperation::Assignment(assignment) => {
                let lhs_ptr = lhs.to_ptr(*location)?;

                let stored_val =
                    assignment.get_stored_value(*location, gen, operand_type, lhs_ptr, rhs)?;

                gen.builder
                    .build_store(lhs_ptr, stored_val.as_basic_value_enum())
                    .to_comp_res(*location)?;

                Ok(Box::new(lhs_ptr))
            }
            BinaryOperation::Math(op) => {
                op.build_instruction(*location, &gen.builder, operand_type, lhs, rhs)
            }
            BinaryOperation::Comparison(cmp) => match operand_type {
                Type::Integer(is_signed, _) => {
                    let pred = cmp.to_llvm_int_predicate(*is_signed);
                    let lhs_int = lhs.to_int(*location)?;
                    let rhs_int = rhs.to_int(*location)?;
                    Ok(Box::new(
                        gen.builder
                            .build_int_compare(pred, lhs_int, rhs_int, "ipred")
                            .to_comp_res(*location)?,
                    ))
                }
                Type::FloatingPoint(_) => {
                    let pred = cmp.to_llvm_float_predicate();
                    let lhs_float = lhs.to_float(*location)?;
                    let rhs_float = rhs.to_float(*location)?;
                    Ok(Box::new(
                        gen.builder
                            .build_float_compare(pred, lhs_float, rhs_float, "fpred")
                            .to_comp_res(*location)?,
                    ))
                }
                Type::RawPtr | Type::TypedPtr(_) => {
                    let pred = cmp.to_llvm_int_predicate(false);
                    let lhs_ptr = lhs.to_ptr(*location)?;
                    let rhs_ptr = rhs.to_ptr(*location)?;
                    Ok(Box::new(
                        gen.builder
                            .build_int_compare(pred, lhs_ptr, rhs_ptr, "ipred")
                            .to_comp_res(*location)?,
                    ))
                }
                Type::Enum(_) => {
                    let pred = cmp.to_llvm_int_predicate(true);
                    let lhs_int = lhs.to_int(*location)?;
                    let rhs_int = rhs.to_int(*location)?;
                    Ok(Box::new(
                        gen.builder
                            .build_int_compare(pred, lhs_int, rhs_int, "ipred")
                            .to_comp_res(*location)?,
                    ))
                }
                _ => compiler_err!(*location, "invalid operation"),
            },
            BinaryOperation::Logical(_) => compiler_err!(
                *location,
                "internal compiler error, this should be handled above"
            ),
        }
    }
}

trait AnyTypeEnumUtil<'ctx> {
    fn to_basic_type(&self) -> Option<BasicTypeEnum<'ctx>>;
}

impl<'ctx> AnyTypeEnumUtil<'ctx> for AnyTypeEnum<'ctx> {
    fn to_basic_type(&self) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            AnyTypeEnum::ArrayType(tp) => Some(BasicTypeEnum::ArrayType(*tp)),
            AnyTypeEnum::FloatType(tp) => Some(BasicTypeEnum::FloatType(*tp)),
            AnyTypeEnum::IntType(tp) => Some(BasicTypeEnum::IntType(*tp)),
            AnyTypeEnum::PointerType(tp) => Some(BasicTypeEnum::PointerType(*tp)),
            AnyTypeEnum::StructType(tp) => Some(BasicTypeEnum::StructType(*tp)),
            AnyTypeEnum::VectorType(tp) => Some(BasicTypeEnum::VectorType(*tp)),
            _ => None,
        }
    }
}

pub struct IRTranslationPass<'irb, 'ctx, 'st> {
    ir_builder: &'irb mut IRBuildContext<'ctx, 'st>,
    import_directory: String,
}

impl<'irb, 'ctx, 'st> IRTranslationPass<'irb, 'ctx, 'st> {
    pub fn new(ir_builder: &'irb mut IRBuildContext<'ctx, 'st>, import_directory: String) -> Self {
        Self {
            ir_builder,
            import_directory,
        }
    }

    pub fn load_value(
        &self,
        node: &IdentifierNode,
        path: &SymbolPath,
        data_type: &Type,
        value_type: &ValueType,
        ptr: &PointerValue<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        match value_type {
            ValueType::LValue => Ok(Box::new(*ptr)),
            ValueType::RValue => Ok(Box::new(self.load_variable(
                &node.location,
                path,
                &data_type,
                ptr,
                node.name.value.as_ref(),
            )?)),
            ValueType::None => Ok(self.ir_builder.null_ptr()),
        }
    }

    fn collect_function_call_args(
        &self,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        fn_type: &FuncTypeBox,
        args: &[ExpressionBox],
        arg_offset: usize,
        call_args: &mut Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> CompilerResult<()> {
        for (i, arg_expr) in args.iter().enumerate() {
            let arg_type = &fn_type.args[i + arg_offset];
            let arg_value = if arg_type.is_ref {
                self.visit_expression(
                    arg_expr.as_ref(),
                    &ExpressionPayload {
                        path: path.clone(),
                        function: function.clone(),
                        expected_type: arg_type.arg_type.clone(),
                        value_type: ValueType::LValue,
                    },
                )?
            } else {
                self.visit_expression(
                    arg_expr.as_ref(),
                    &ExpressionPayload {
                        path: path.clone(),
                        function: function.clone(),
                        expected_type: arg_type.arg_type.clone(),
                        value_type: ValueType::RValue,
                    },
                )?
            };

            let arg_value_enum = arg_value.as_basic_value_enum();
            call_args.push(arg_value_enum.into());
        }
        Ok(())
    }

    fn translate_scope(
        &mut self,
        node: &ScopeNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<BasicBlock<'ctx>> {
        let basic_block = self
            .ir_builder
            .context
            .append_basic_block(pd.function, &node.name);
        self.ir_builder.builder.position_at_end(basic_block);

        self.visit_scope_statements(node, pd)?;

        Ok(basic_block)
    }

    fn visit_scope_statements(
        &mut self,
        node: &ScopeNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<()> {
        for stmt in &node.body {
            self.visit_statement(stmt.as_ref(), pd)?;
        }
        Ok(())
    }

    fn deduce_type(
        &self,
        path: SymbolPath,
        expected_type: Type,
        node: &AnyExpressionNode,
    ) -> CompilerResult<Type> {
        deduce_type(self.ir_builder.symbol_table, path, expected_type, node)
    }

    fn evaluate_compile_time_value(
        &self,
        path: SymbolPath,
        expected_type: Type,
        node: &AnyExpressionNode,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        crate::ast_passes::compile_time_evaluation_pass::CompileTimeEvaluationPass::new(
            self.ir_builder,
        )
        .visit_expression(
            node,
            &crate::ast_passes::compile_time_evaluation_pass::Payload {
                path,
                expected_type,
            },
        )
    }

    fn build_cast(
        &self,
        location: Location,
        source_type: &Type,
        target_type: &Type,
        value: Box<dyn BasicValue<'ctx> + 'ctx>,
    ) -> CompilerResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if *source_type == *target_type {
            return Ok(value);
        }

        let source_llvm_type = source_type
            .to_llvm_basic_type(&self.ir_builder.context)
            .to_comp_res_with_desc(location, "invalid source type")?;

        let target_llvm_type = target_type
            .to_llvm_basic_type(&self.ir_builder.context)
            .to_comp_res_with_desc(location, "invalid target type")?;

        match source_type {
            Type::Integer(src_is_signed, src_data_size) => {
                let value_int = value.to_int(location)?;
                match target_type {
                    Type::Integer(_, dst_data_size) => {
                        if dst_data_size.bit_count() > src_data_size.bit_count() {
                            Ok(Box::new(
                                if *src_is_signed {
                                    self.ir_builder.builder.build_int_s_extend(
                                        value_int,
                                        target_llvm_type.into_int_type(),
                                        "sextended",
                                    )
                                } else {
                                    self.ir_builder.builder.build_int_z_extend(
                                        value_int,
                                        target_llvm_type.into_int_type(),
                                        "zextended",
                                    )
                                }
                                .to_comp_res(location)?,
                            ))
                        } else if dst_data_size.bit_count() < src_data_size.bit_count() {
                            Ok(Box::new(
                                self.ir_builder
                                    .builder
                                    .build_int_truncate(
                                        value_int,
                                        target_llvm_type.into_int_type(),
                                        "trunced",
                                    )
                                    .to_comp_res(location)?,
                            ))
                        } else {
                            Ok(value)
                        }
                    }
                    Type::FloatingPoint(_) => Ok(Box::new(
                        if *src_is_signed {
                            self.ir_builder.builder.build_signed_int_to_float(
                                value_int,
                                target_llvm_type.into_float_type(),
                                "fscast",
                            )
                        } else {
                            self.ir_builder.builder.build_unsigned_int_to_float(
                                value_int,
                                target_llvm_type.into_float_type(),
                                "fucast",
                            )
                        }
                        .to_comp_res(location)?,
                    )),
                    Type::Bool => Ok(Box::new(
                        self.ir_builder
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                value_int,
                                source_llvm_type.into_int_type().const_int(0, false),
                                "intcmp",
                            )
                            .to_comp_res(location)?,
                    )),
                    Type::Enum(_) => {
                        if *src_data_size == DataSize::Bits32 {
                            Ok(value)
                        } else {
                            compiler_err!(location, "incompatible enum type")
                        }
                    }
                    _ => compiler_err!(location, "invalid cast target type"),
                }
            }
            Type::FloatingPoint(src_data_size) => {
                let value_float = value.to_float(location)?;
                match target_type {
                    Type::Integer(dst_is_signed, _) => Ok(Box::new(
                        if *dst_is_signed {
                            self.ir_builder.builder.build_float_to_signed_int(
                                value_float,
                                target_llvm_type.into_int_type(),
                                "icast",
                            )
                        } else {
                            self.ir_builder.builder.build_float_to_unsigned_int(
                                value_float,
                                target_llvm_type.into_int_type(),
                                "ucast",
                            )
                        }
                        .to_comp_res(location)?,
                    )),
                    Type::FloatingPoint(dst_data_size) => {
                        if dst_data_size.bit_count() > src_data_size.bit_count() {
                            Ok(Box::new(
                                self.ir_builder
                                    .builder
                                    .build_float_ext(
                                        value_float,
                                        target_llvm_type.into_float_type(),
                                        "floatext",
                                    )
                                    .to_comp_res(location)?,
                            ))
                        } else {
                            Ok(Box::new(
                                self.ir_builder
                                    .builder
                                    .build_float_trunc(
                                        value_float,
                                        target_llvm_type.into_float_type(),
                                        "floattrunc",
                                    )
                                    .to_comp_res(location)?,
                            ))
                        }
                    }
                    _ => compiler_err!(location, "invalid cast target type"),
                }
            }
            Type::Enum(_) => match target_type {
                Type::Integer(_, data_size) => {
                    if *data_size == DataSize::Bits32 {
                        Ok(value)
                    } else {
                        compiler_err!(location, "invalid cast source type")
                    }
                }
                _ => compiler_err!(location, "invalid cast source type"),
            },
            _ => compiler_err!(location, "invalid cast source type"),
        }
    }

    fn translate_with_cast(
        &self,
        node: &AnyExpressionNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let stmt = self.visit_expression(node, pd)?;
        if pd.value_type == ValueType::LValue {
            Ok(stmt)
        } else {
            let deduced_type_raw =
                self.deduce_type(pd.path.clone(), pd.expected_type.clone(), node)?;
            let deduced_type = self
                .ir_builder
                .symbol_table
                .resolve_type_alias(&pd.path, deduced_type_raw)
                .to_comp_res(*node.get_location())?;
            let expected_type_resolved = self
                .ir_builder
                .symbol_table
                .resolve_type_alias(&pd.path, pd.expected_type.clone())
                .to_comp_res(*node.get_location())?;
            self.build_cast(
                *node.get_location(),
                &deduced_type,
                &expected_type_resolved,
                stmt,
            )
        }
    }

    fn build_boolean_branch(
        &self,
        node: &AnyExpressionNode,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        let value = self.translate_with_cast(
            node,
            &ExpressionPayload {
                path: path.clone(),
                function: *function,
                expected_type: Type::Bool,
                value_type: ValueType::RValue,
            },
        )?;
        let value_int = value.as_ref().to_int(*node.get_location())?;
        self.ir_builder
            .builder
            .build_conditional_branch(value_int, true_block, false_block)
            .to_comp_res(*node.get_location())?;
        Ok(())
    }

    fn translate_binary_expression_to_boolean(
        &self,
        node: &BinaryExpressionNode,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        if let BinaryOperation::Logical(operation) = node.operation {
            match operation {
                LogicalBinaryOperation::LogicalAnd => {
                    let and_block = self
                        .ir_builder
                        .context
                        .append_basic_block(*function, "and.pred");
                    self.translate_to_boolean(
                        node.left.as_ref(),
                        path,
                        function,
                        and_block,
                        false_block,
                    )?;
                    self.ir_builder.builder.position_at_end(and_block);
                    self.translate_to_boolean(
                        node.right.as_ref(),
                        path,
                        function,
                        true_block,
                        false_block,
                    )?;
                }
                LogicalBinaryOperation::LogicalOr => {
                    let or_block = self
                        .ir_builder
                        .context
                        .append_basic_block(*function, "or.pred");
                    self.translate_to_boolean(
                        node.left.as_ref(),
                        path,
                        function,
                        true_block,
                        or_block,
                    )?;
                    self.ir_builder.builder.position_at_end(or_block);
                    self.translate_to_boolean(
                        node.right.as_ref(),
                        path,
                        function,
                        true_block,
                        false_block,
                    )?;
                }
            }
        } else {
            self.build_boolean_branch(&node.into(), path, function, true_block, false_block)?;
        }

        Ok(())
    }
    fn translate_singular_expression_to_boolean(
        &self,
        node: &SingularExpressionNode,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        match node.operation {
            SingularOperation::Not => {
                self.translate_to_boolean(
                    node.expr.as_ref(),
                    path,
                    function,
                    false_block,
                    true_block,
                )?;
            }
            _ => {
                self.build_boolean_branch(&node.into(), path, function, true_block, false_block)?;
            }
        };

        Ok(())
    }

    fn translate_to_boolean(
        &self,
        node: &AnyExpressionNode,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        match node {
            AnyExpressionNode::BinaryExpression(binary_expr_node) => self
                .translate_binary_expression_to_boolean(
                    binary_expr_node,
                    path,
                    function,
                    true_block,
                    false_block,
                ),
            AnyExpressionNode::SingularExpression(singular_expr_node) => self
                .translate_singular_expression_to_boolean(
                    singular_expr_node,
                    path,
                    function,
                    true_block,
                    false_block,
                ),
            _ => self.build_boolean_branch(node, path, function, true_block, false_block),
        }
    }

    pub fn translate_type(
        &self,
        location: &Location,
        path: &SymbolPath,
        src_type: &Type,
    ) -> CompilerResult<AnyTypeEnum<'ctx>> {
        if let Type::Alias(aliased_type) = src_type {
            let alias_path = self
                .ir_builder
                .symbol_table
                .get_identifier_path(path, aliased_type)
                .to_comp_res(*location)?;
            let resolved_type = self
                .ir_builder
                .ir_value_storage
                .find_global_type(&alias_path, aliased_type.value.as_ref());
            if let Some(res_type) = resolved_type {
                return Ok(res_type);
            }
        };

        let resolved_type = self
            .ir_builder
            .symbol_table
            .resolve_type_alias(path, src_type.clone())
            .to_comp_res(*location)?;

        Ok(visit_type!(
            *location,
            self.ir_builder.context,
            &resolved_type,
            value,
            Ok(value.into())
        )?)
    }

    pub fn build_get_element_ptr(
        &self,
        location: Location,
        path: &SymbolPath,
        ptr_type: &Type,
        ptr: PointerValue<'ctx>,
        indices: &[IntValue<'ctx>],
        name: &str,
    ) -> CompilerResult<PointerValue<'ctx>> {
        let translated_type = self
            .translate_type(&location, path, ptr_type)?
            .to_basic_type()
            .to_comp_res_with_desc(location, "invalid type")?;

        unsafe {
            self.ir_builder
                .builder
                .build_gep(translated_type, ptr, indices, name)
                .to_comp_res(location)
        }
    }

    pub fn load_variable(
        &self,
        location: &Location,
        path: &SymbolPath,
        var_type: &Type,
        ptr: &PointerValue<'ctx>,
        name: &str,
    ) -> CompilerResult<BasicValueEnum<'ctx>> {
        let translated_type = self
            .translate_type(location, path, var_type)?
            .to_basic_type()
            .to_comp_res_with_desc(*location, "invalid type")?;

        self.ir_builder
            .builder
            .build_load(translated_type, *ptr, name)
            .to_comp_res(*location)
    }

    pub fn allocate_variable(
        &self,
        location: &Location,
        path: &SymbolPath,
        var_type: &Type,
        name: &str,
    ) -> CompilerResult<PointerValue<'ctx>> {
        let translated_type = self
            .translate_type(location, path, var_type)?
            .to_basic_type()
            .to_comp_res_with_desc(*location, "invalid type")?;

        self.ir_builder
            .builder
            .build_alloca(translated_type, name)
            .to_comp_res(*location)
    }

    pub fn build_match_case_condition(
        &self,
        cond_type: &Type,
        expr: &AnyExpressionNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if let Type::Enum(enum_type) = cond_type {
            match expr {
                AnyExpressionNode::Identifier(identifier) => {
                    let opt_index = enum_type.enumerations.get(identifier.name.value.as_str());
                    if let Some(index) = opt_index {
                        return Ok(Box::new(
                            self.ir_builder
                                .context
                                .i32_type()
                                .const_int(*index as u64, true),
                        ));
                    }
                }
                _ => {}
            }
        }

        self.translate_with_cast(expr, pd)
    }
}

impl<'irb, 'ctx, 'st> GlobalStatementVisitor for IRTranslationPass<'irb, 'ctx, 'st> {
    type Payload = SymbolPath;
    type VisitResult = CompilerResult<()>;

    fn visit_source_unit(&mut self, node: &SourceUnit, path: &SymbolPath) -> CompilerResult<()> {
        for stmt in &node.body {
            self.visit_global_statement(stmt.as_ref(), path)?;
        }
        Ok(())
    }

    fn visit_const_decl(&mut self, node: &ConstDeclNode, path: &SymbolPath) -> CompilerResult<()> {
        let sym_path = path.sub(&node.name);
        let sym = self
            .ir_builder
            .symbol_table
            .find_by_path(&sym_path)
            .to_comp_res_with_desc(node.location, "internal error")?;
        let basic_value = self.evaluate_compile_time_value(
            path.clone(),
            sym.data_type.clone(),
            node.value.as_ref(),
        )?;
        self.ir_builder
            .ir_value_storage
            .register_basic_value(sym_path, basic_value);
        Ok(())
    }

    fn visit_function(&mut self, node: &FunctionNode, path: &SymbolPath) -> CompilerResult<()> {
        let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for param in &node.params {
            if param.is_ref {
                args.push(
                    self.ir_builder
                        .context
                        .ptr_type(AddressSpace::from(0))
                        .into(),
                );
            } else {
                args.push(
                    self.translate_type(&node.location, path, &param.arg_type)?
                        .to_basic_type()
                        .to_comp_res_with_desc(node.location, "invalid type")?
                        .into(),
                );
            }
        }

        let resolved_ret_type = self
            .ir_builder
            .symbol_table
            .resolve_type_alias(path, node.ret_type.clone())
            .to_comp_res(node.location)?;

        let fn_type = visit_any_type!(
            node.location,
            self.ir_builder.context,
            &resolved_ret_type,
            value,
            Ok(value.fn_type(&args[..], false))
        )?;

        let linkage = match node.linkage {
            FunctionLinkage::Standard | FunctionLinkage::Entrypoint => None,
            FunctionLinkage::External => Some(Linkage::External),
        };

        let sub_path = node.sub_path(node.location, self.ir_builder.symbol_table, path)?;

        let function =
            self.ir_builder
                .module
                .add_function(&node.effective_name(&sub_path), fn_type, linkage);
        self.ir_builder
            .ir_value_storage
            .register_func(sub_path.clone(), function);

        match &node.scope {
            Some(scope) => {
                let basic_block = self
                    .ir_builder
                    .context
                    .append_basic_block(function, &node.name);
                self.ir_builder.builder.position_at_end(basic_block);

                for (path, sym) in self.ir_builder.symbol_table.iterate_path(&sub_path) {
                    match sym.sym_type {
                        SymbolType::LocalVariable => {
                            let translated_type = self
                                .translate_type(&sym.location, path, &sym.data_type)?
                                .to_basic_type()
                                .to_comp_res_with_desc(sym.location, "invalid type")?;
                            let addr = self
                                .ir_builder
                                .builder
                                .build_alloca(translated_type, &sym.name)
                                .to_comp_res(sym.location)?;

                            self.ir_builder
                                .ir_value_storage
                                .register_ptr(path.clone(), addr);
                        }
                        SymbolType::LocalReference => {
                            let addr = self.allocate_variable(
                                &sym.location,
                                &path,
                                &Type::RawPtr,
                                &sym.name,
                            )?;
                            self.ir_builder
                                .ir_value_storage
                                .register_ptr(path.clone(), addr);
                        }
                        _ => {}
                    }
                }

                self.visit_scope_statements(
                    scope,
                    &StatementPayload {
                        path: sub_path.clone(),
                        function,
                    },
                )?;
            }
            None => {}
        }

        Ok(())
    }

    fn visit_struct(&mut self, node: &StructNode, path: &SymbolPath) -> CompilerResult<()> {
        let struct_path = path.sub(&node.name);
        let symbol = self
            .ir_builder
            .symbol_table
            .find_by_path(&struct_path)
            .to_comp_res_with_desc(node.location, "failed to find struct symbol")?;

        if let Type::Struct(struct_type) = &symbol.data_type {
            let struct_name = format!("perun.struct.{}", struct_path.to_string());
            let ir_type = self
                .ir_builder
                .context
                .opaque_struct_type(struct_name.as_ref());

            let mut basic_types: Vec<BasicTypeEnum> = Vec::new();
            for field in &struct_type.fields {
                basic_types.push(
                    field
                        .to_llvm_basic_type(self.ir_builder.context)
                        .to_comp_res_with_desc(node.location, "failed to deduce type")?,
                );
            }

            ir_type.set_body(basic_types.as_slice(), false);

            self.ir_builder
                .ir_value_storage
                .register_type(struct_path, ir_type.into());

            Ok(())
        } else {
            compiler_err!(node.location, "failed struct")
        }
    }

    fn visit_import(&mut self, node: &ImportNode, _: &SymbolPath) -> CompilerResult<()> {
        let module = Module::new(
            &format!(
                "{}/{}.json",
                self.import_directory.as_str(),
                node.module_name
            ),
            node.location,
        )
        .to_comp_res_with_desc(node.location, "unable to load module")?;

        let module_path = SymbolPath::new(node.module_name.as_ref());

        for struct_node in &module.structs {
            self.visit_struct(struct_node, &module_path)?;
        }
        for constant_node in &module.constants {
            self.visit_const_decl(constant_node, &module_path)?;
        }
        for func_node in &module.functions {
            self.visit_function(func_node, &module_path)?;
        }
        for enum_node in &module.enums {
            self.visit_enum(enum_node, &module_path)?;
        }

        Ok(())
    }

    fn visit_enum(&mut self, node: &EnumNode, pd: &SymbolPath) -> Self::VisitResult {
        let enum_path = pd.sub(&node.name);
        for (i, enumeration) in node.enumerations.iter().enumerate() {
            self.ir_builder.ir_value_storage.register_basic_value(
                enum_path.sub(enumeration),
                Box::new(self.ir_builder.context.i32_type().const_int(i as u64, true)),
            );
        }

        Ok(())
    }
}

pub struct StatementPayload<'ctx> {
    path: SymbolPath,
    function: FunctionValue<'ctx>,
}

impl<'irb, 'ctx, 'st> StatementVisitor for IRTranslationPass<'irb, 'ctx, 'st> {
    type Payload = StatementPayload<'ctx>;
    type VisitResult = CompilerResult<()>;

    fn visit_return_node(
        &mut self,
        node: &ReturnNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<()> {
        let msg = format!("unable to find function type {}", pd.path);
        let func_symbol = self
            .ir_builder
            .symbol_table
            .find_by_path(&pd.path)
            .to_comp_res_with_desc(node.location, &msg)?;

        if let Type::Function(func_type) = &func_symbol.data_type {
            match &node.expression {
                Some(expr) => {
                    let value = self.translate_with_cast(
                        expr.as_ref(),
                        &ExpressionPayload {
                            path: pd.path.clone(),
                            function: pd.function,
                            expected_type: func_type.ret_type.clone(),
                            value_type: ValueType::RValue,
                        },
                    )?;
                    self.ir_builder
                        .builder
                        .build_return(Some(value.as_ref()))
                        .to_comp_res(node.location)?;
                }
                None => {
                    self.ir_builder
                        .builder
                        .build_return(None)
                        .to_comp_res(node.location)?;
                }
            }
            return Ok(());
        }

        compiler_err!(node.location, "invalid type");
    }

    fn visit_var_decl_node(
        &mut self,
        node: &VarDeclNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<()> {
        if let Some(expr) = &node.expression {
            let symbol = self
                .ir_builder
                .symbol_table
                .find_symbol(&pd.path, &node.name)
                .to_comp_res(node.location)?;
            let addr = *self
                .ir_builder
                .ir_value_storage
                .find_symbol(&pd.path, &node.name)
                .to_comp_res(node.location)?;

            let value = self.translate_with_cast(
                expr,
                &ExpressionPayload {
                    path: pd.path.clone(),
                    function: pd.function,
                    expected_type: symbol.data_type.clone(),
                    value_type: ValueType::RValue,
                },
            )?;

            self.ir_builder
                .builder
                .build_store(addr, value.as_ref().as_basic_value_enum())
                .to_comp_res(node.location)?;
        }
        Ok(())
    }

    fn visit_ref_decl_node(
        &mut self,
        node: &RefDeclNode,
        pd: &Self::Payload,
    ) -> CompilerResult<()> {
        let symbol = self
            .ir_builder
            .symbol_table
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;
        let addr = *self
            .ir_builder
            .ir_value_storage
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;

        let value = self.visit_expression(
            node.expression.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: symbol.data_type.clone(),
                value_type: ValueType::LValue,
            },
        )?;

        let ptr_value = value.as_ref().to_ptr(node.location)?;
        self.ir_builder
            .builder
            .build_store(addr, ptr_value)
            .to_comp_res(node.location)?;

        Ok(())
    }

    fn visit_if_node(&mut self, node: &IfNode, pd: &StatementPayload<'ctx>) -> CompilerResult<()> {
        let current_block = self
            .ir_builder
            .builder
            .get_insert_block()
            .to_comp_res_with_desc(node.location, "statement not located in a valid block")?;

        let if_true_block = self.translate_scope(
            &node.then_scope,
            &StatementPayload {
                path: pd.path.sub(&node.then_scope.name),
                function: pd.function,
            },
        )?;
        let if_end_block = self
            .ir_builder
            .context
            .append_basic_block(pd.function, "if.end");
        self.ir_builder
            .builder
            .build_unconditional_branch(if_end_block)
            .to_comp_res(node.location)?;

        let if_false_block = if let Some(else_scope) = &node.else_scope {
            let scope = self.translate_scope(
                else_scope,
                &StatementPayload {
                    path: pd.path.sub(&else_scope.name),
                    function: pd.function,
                },
            )?;
            self.ir_builder
                .builder
                .build_unconditional_branch(if_end_block)
                .to_comp_res(node.location)?;
            scope
        } else {
            if_end_block
        };

        self.ir_builder.builder.position_at_end(current_block);
        self.translate_to_boolean(
            node.condition.as_ref(),
            &pd.path,
            &pd.function,
            if_true_block,
            if_false_block,
        )?;

        self.ir_builder.builder.position_at_end(if_end_block);

        Ok(())
    }

    fn visit_while_node(
        &mut self,
        node: &WhileNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<()> {
        let while_cond = self
            .ir_builder
            .context
            .append_basic_block(pd.function, "while.cond");
        self.ir_builder
            .builder
            .build_unconditional_branch(while_cond)
            .to_comp_res(node.location)?;
        self.ir_builder.builder.position_at_end(while_cond);

        let while_body = self.translate_scope(
            &node.scope,
            &StatementPayload {
                path: pd.path.sub(&node.scope.name),
                function: pd.function,
            },
        )?;

        self.ir_builder
            .builder
            .build_unconditional_branch(while_cond)
            .to_comp_res(node.location)?;

        let while_end = self
            .ir_builder
            .context
            .append_basic_block(pd.function, "while.end");

        self.ir_builder.builder.position_at_end(while_cond);
        self.translate_to_boolean(
            node.condition.as_ref(),
            &pd.path,
            &pd.function,
            while_body,
            while_end,
        )?;

        self.ir_builder.builder.position_at_end(while_end);

        Ok(())
    }

    fn visit_match_node(&mut self, node: &MatchNode, pd: &Self::Payload) -> CompilerResult<()> {
        let expr_type_unresolved = self.deduce_type(
            pd.path.clone(),
            Type::Integer(true, DataSize::Bits32),
            node.expression.as_ref(),
        )?;
        let expr_type = self
            .ir_builder
            .symbol_table
            .resolve_type_alias(&pd.path, expr_type_unresolved)
            .to_comp_res(node.location)?;

        let expr = self.translate_with_cast(
            node.expression.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: Type::Integer(true, DataSize::Bits32),
                value_type: ValueType::RValue,
            },
        )?;

        let current_block = self
            .ir_builder
            .builder
            .get_insert_block()
            .to_comp_res_with_desc(node.location, "statement not located in a valid block")?;

        let epilogue_block = self
            .ir_builder
            .context
            .append_basic_block(pd.function, "match.epilogue");

        let mut cases = Vec::<(IntValue<'ctx>, BasicBlock<'ctx>)>::new();
        for case in &node.cases {
            let cond = self.build_match_case_condition(
                &expr_type,
                case.condition.as_ref(),
                &ExpressionPayload {
                    path: pd.path.clone(),
                    function: pd.function,
                    expected_type: Type::Integer(true, DataSize::Bits32),
                    value_type: ValueType::RValue,
                },
            )?;

            let block = self.translate_scope(
                &case.scope,
                &StatementPayload {
                    path: pd.path.sub(case.scope.name.as_str()),
                    function: pd.function,
                },
            )?;

            self.ir_builder
                .builder
                .build_unconditional_branch(epilogue_block)
                .to_comp_res(node.location)?;

            cases.push((cond.to_int(*case.condition.get_location())?, block));
        }

        let else_block = if let Some(def_case) = &node.default_case {
            let block = self.translate_scope(
                def_case,
                &StatementPayload {
                    path: pd.path.sub(def_case.name.as_str()),
                    function: pd.function,
                },
            )?;

            self.ir_builder
                .builder
                .build_unconditional_branch(epilogue_block)
                .to_comp_res(node.location)?;

            block
        } else {
            epilogue_block
        };

        self.ir_builder.builder.position_at_end(current_block);

        self.ir_builder
            .builder
            .build_switch(expr.to_int(node.location)?, else_block, cases.as_slice())
            .to_comp_res(node.location)?;

        self.ir_builder.builder.position_at_end(epilogue_block);

        Ok(())
    }

    fn visit_expression_statement_node(
        &mut self,
        node: &ExpressionStatementNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<()> {
        self.visit_expression(
            node.expression.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: Type::Void,
                value_type: ValueType::None,
            },
        )?;
        Ok(())
    }
}

pub struct ExpressionPayload<'ctx> {
    path: SymbolPath,
    function: FunctionValue<'ctx>,
    expected_type: Type,
    value_type: ValueType,
}

pub type BasicValueBox<'ctx> = Box<dyn BasicValue<'ctx> + 'ctx>;

impl<'irb, 'ctx, 'st> ExpressionVisitor for IRTranslationPass<'irb, 'ctx, 'st> {
    type Payload = ExpressionPayload<'ctx>;
    type VisitResult = CompilerResult<BasicValueBox<'ctx>>;

    fn visit_identifier(
        &self,
        node: &IdentifierNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let path = self
            .ir_builder
            .symbol_table
            .get_identifier_path(&pd.path, &node.name)
            .to_comp_res(node.location)?;

        let symbol = self
            .ir_builder
            .symbol_table
            .find_symbol(&path, &node.name.value)
            .to_comp_res(node.location)?;
        match symbol.sym_type {
            SymbolType::FunctionArg(index, is_ref) => {
                let arg_expr = pd
                    .function
                    .get_nth_param(index as u32)
                    .to_comp_res_with_desc(symbol.location, "invalid function argument")?;
                if is_ref {
                    let arg_ptr = arg_expr.to_ptr(symbol.location)?;
                    match pd.value_type {
                        ValueType::LValue => Ok(Box::new(arg_ptr)),
                        ValueType::RValue => Ok(Box::new(self.load_variable(
                            &node.location,
                            &path,
                            &symbol.data_type,
                            &arg_ptr,
                            node.name.value.as_ref(),
                        )?)),
                        ValueType::None => Ok(self.ir_builder.null_ptr()),
                    }
                } else {
                    Ok(Box::new(arg_expr))
                }
            }
            SymbolType::LocalVariable => {
                let (sym, ptr) = self.ir_builder.find_symbol_with_addr(
                    node.location,
                    &path,
                    node.name.value.as_ref(),
                )?;
                let data_type = self
                    .ir_builder
                    .symbol_table
                    .resolve_type_alias(&path, sym.data_type.clone())
                    .to_comp_res(node.location)?;
                self.load_value(node, &path, &data_type, &pd.value_type, ptr)
            }
            SymbolType::LocalReference => {
                let (sym, ptr) = self.ir_builder.find_symbol_with_addr(
                    node.location,
                    &path,
                    node.name.value.as_ref(),
                )?;
                let loaded_ptr_var = self.load_variable(
                    &node.location,
                    &path,
                    &Type::RawPtr,
                    ptr,
                    node.name.value.as_ref(),
                )?;
                let loaded_ptr = loaded_ptr_var.to_ptr(node.location)?;
                self.load_value(node, &path, &sym.data_type, &pd.value_type, &loaded_ptr)
            }
            SymbolType::ConstantDef => {
                let basic_val = self
                    .ir_builder
                    .ir_value_storage
                    .find_basic_value(&path, &node.name.value)
                    .to_comp_res_with_desc(node.location, "unable to find value")?;

                if pd.value_type == ValueType::LValue {
                    let llvm_type = symbol
                        .data_type
                        .to_llvm_basic_type(self.ir_builder.context)
                        .to_comp_res_with_desc(node.location, "cannot map type")?;
                    let global_val = self.ir_builder.module.add_global(
                        llvm_type,
                        Some(AddressSpace::from(0)),
                        &node.name.value,
                    );
                    global_val.set_constant(true);
                    global_val.set_initializer(basic_val);
                    Ok(Box::new(global_val))
                } else {
                    Ok(Box::new(basic_val.as_basic_value_enum()))
                }
            }
            _ => {
                compiler_err!(node.location, "TODO: Implement")
            }
        }
    }

    fn visit_null(
        &self,
        _: &NullNode,
        _: &ExpressionPayload,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        Ok(self.ir_builder.null_ptr())
    }

    fn visit_boolean(
        &self,
        node: &BooleanNode,
        pd: &ExpressionPayload,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        assert_eq!(pd.value_type, ValueType::RValue);
        self.evaluate_compile_time_value(pd.path.clone(), pd.expected_type.clone(), &node.into())
    }

    fn visit_self(
        &self,
        node: &SelfNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let sym = self
            .ir_builder
            .symbol_table
            .find_symbol(&pd.path, "self")
            .to_comp_res(node.location)?;
        if let SymbolType::FunctionArg(index, _) = sym.sym_type {
            let arg_expr = pd
                .function
                .get_nth_param(index as u32)
                .to_comp_res_with_desc(sym.location, "invalid function argument")?;
            Ok(Box::new(arg_expr))
        } else {
            compiler_err!(node.location, "self is defined incorrectly");
        }
    }

    fn visit_number(
        &self,
        node: &NumberNode,
        pd: &ExpressionPayload,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        assert_eq!(pd.value_type, ValueType::RValue);
        self.evaluate_compile_time_value(pd.path.clone(), pd.expected_type.clone(), &node.into())
    }

    fn visit_floating_point(
        &self,
        node: &FloatingPointNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult {
        assert_eq!(pd.value_type, ValueType::RValue);
        self.evaluate_compile_time_value(pd.path.clone(), pd.expected_type.clone(), &node.into())
    }

    fn visit_string(
        &self,
        node: &StringNode,
        pd: &ExpressionPayload,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if pd.value_type == ValueType::LValue {
            compiler_err!(node.location, "tried to interpret a string as an l-value");
        }
        self.evaluate_compile_time_value(pd.path.clone(), pd.expected_type.clone(), &node.into())
    }

    fn visit_binary_expression(
        &self,
        node: &BinaryExpressionNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult {
        if pd.value_type == ValueType::LValue {
            compiler_err!(
                node.location,
                "tried to interpret assignment expression as an l-value"
            );
        }

        if let BinaryOperation::Logical(logical_op) = node.operation {
            match logical_op {
                LogicalBinaryOperation::LogicalAnd => {
                    let and_block = self
                        .ir_builder
                        .context
                        .append_basic_block(pd.function, "and.pred");
                    let end_block = self
                        .ir_builder
                        .context
                        .append_basic_block(pd.function, "end.pred");

                    let left = self.translate_with_cast(
                        node.left.as_ref(),
                        &ExpressionPayload {
                            path: pd.path.clone(),
                            function: pd.function,
                            expected_type: Type::Bool,
                            value_type: ValueType::RValue,
                        },
                    )?;
                    let left_int = left.as_ref().to_int(*node.get_location())?;
                    let left_block = self
                        .ir_builder
                        .builder
                        .get_insert_block()
                        .to_comp_res_with_desc(node.location, "invalid block")?;
                    self.ir_builder
                        .builder
                        .build_conditional_branch(left_int, and_block, end_block)
                        .to_comp_res(*node.get_location())?;

                    self.ir_builder.builder.position_at_end(and_block);

                    let right = self.translate_with_cast(
                        node.right.as_ref(),
                        &ExpressionPayload {
                            path: pd.path.clone(),
                            function: pd.function,
                            expected_type: Type::Bool,
                            value_type: ValueType::RValue,
                        },
                    )?;

                    let right_block = self
                        .ir_builder
                        .builder
                        .get_insert_block()
                        .to_comp_res_with_desc(node.location, "invalid block")?;
                    self.ir_builder
                        .builder
                        .build_unconditional_branch(end_block)
                        .to_comp_res(node.location)?;

                    self.ir_builder.builder.position_at_end(end_block);

                    let result = self
                        .ir_builder
                        .builder
                        .build_phi(self.ir_builder.context.bool_type(), "and.result")
                        .to_comp_res(node.location)?;
                    result.add_incoming(&[
                        (
                            &self.ir_builder.context.bool_type().const_zero(),
                            left_block,
                        ),
                        (right.as_ref(), right_block),
                    ]);

                    Ok(Box::new(result.as_basic_value()))
                }
                LogicalBinaryOperation::LogicalOr => {
                    let or_block = self
                        .ir_builder
                        .context
                        .append_basic_block(pd.function, "or.pred");
                    let end_block = self
                        .ir_builder
                        .context
                        .append_basic_block(pd.function, "end.pred");

                    let left = self.translate_with_cast(
                        node.left.as_ref(),
                        &ExpressionPayload {
                            path: pd.path.clone(),
                            function: pd.function,
                            expected_type: Type::Bool,
                            value_type: ValueType::RValue,
                        },
                    )?;

                    let left_int = left.as_ref().to_int(*node.get_location())?;
                    let left_block = self
                        .ir_builder
                        .builder
                        .get_insert_block()
                        .to_comp_res_with_desc(node.location, "invalid block")?;
                    self.ir_builder
                        .builder
                        .build_conditional_branch(left_int, end_block, or_block)
                        .to_comp_res(*node.get_location())?;

                    self.ir_builder.builder.position_at_end(or_block);

                    let right = self.translate_with_cast(
                        node.right.as_ref(),
                        &ExpressionPayload {
                            path: pd.path.clone(),
                            function: pd.function,
                            expected_type: Type::Bool,
                            value_type: ValueType::RValue,
                        },
                    )?;
                    let right_block = self
                        .ir_builder
                        .builder
                        .get_insert_block()
                        .to_comp_res_with_desc(node.location, "invalid block")?;
                    self.ir_builder
                        .builder
                        .build_unconditional_branch(end_block)
                        .to_comp_res(node.location)?;

                    self.ir_builder.builder.position_at_end(end_block);

                    let result = self
                        .ir_builder
                        .builder
                        .build_phi(self.ir_builder.context.bool_type(), "or.result")
                        .to_comp_res(node.location)?;
                    result.add_incoming(&[
                        (
                            &self.ir_builder.context.bool_type().const_int(1, false),
                            left_block,
                        ),
                        (right.as_ref(), right_block),
                    ]);

                    Ok(Box::new(result.as_basic_value()))
                }
            }
        } else {
            // For assigment we always expect the left hand side to be an l-value.
            let left_value_type = node.get_left_value_type();

            let (operand_type_raw, _) = TypeDeductionPass::new(self.ir_builder.symbol_table)
                .deduce_operand_and_out_type_for_binary_expression(
                    node,
                    &crate::ast_passes::type_deduction_pass::Payload {
                        path: pd.path.clone(),
                        expected_type: pd.expected_type.clone(),
                    },
                )?;

            let operand_type = self
                .ir_builder
                .symbol_table
                .resolve_type_alias(&pd.path, operand_type_raw)
                .to_comp_res(node.location)?;

            let left = self.translate_with_cast(
                node.left.as_ref(),
                &ExpressionPayload {
                    path: pd.path.clone(),
                    function: pd.function,
                    expected_type: operand_type.clone(),
                    value_type: left_value_type,
                },
            )?;
            let right = self.translate_with_cast(
                node.right.as_ref(),
                &ExpressionPayload {
                    path: pd.path.clone(),
                    function: pd.function,
                    expected_type: operand_type.clone(),
                    value_type: ValueType::RValue,
                },
            )?;

            node.operation.translate(
                self.ir_builder,
                &node.location,
                &operand_type,
                left.as_ref().as_basic_value_enum(),
                right.as_ref().as_basic_value_enum(),
            )
        }
    }

    fn visit_singular_expression(
        &self,
        node: &SingularExpressionNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        match node.operation {
            SingularOperation::AddressOf => {
                if pd.value_type == ValueType::LValue {
                    compiler_err!(
                        node.location,
                        "tried to interpret assignment expression as an l-value"
                    );
                }
                self.visit_expression(
                    node.expr.as_ref(),
                    &ExpressionPayload {
                        path: pd.path.clone(),
                        function: pd.function,
                        expected_type: pd.expected_type.clone(),
                        value_type: ValueType::LValue,
                    },
                )
            }
            SingularOperation::Deference => {
                let ptr_box = self.visit_expression(
                    node.expr.as_ref(),
                    &ExpressionPayload {
                        path: pd.path.clone(),
                        function: pd.function,
                        expected_type: Type::RawPtr,
                        value_type: ValueType::RValue,
                    },
                )?;
                match pd.value_type {
                    ValueType::LValue => Ok(ptr_box),
                    ValueType::RValue => {
                        let ptr = ptr_box.as_ref().to_ptr(node.location)?;
                        Ok(Box::new(self.load_variable(
                            &node.location,
                            &pd.path,
                            &pd.expected_type,
                            &ptr,
                            "deref",
                        )?))
                    }
                    ValueType::None => Ok(self.ir_builder.null_ptr()),
                }
            }
            SingularOperation::Not => {
                let expr_value = self.translate_with_cast(
                    node.expr.as_ref(),
                    &ExpressionPayload {
                        path: pd.path.clone(),
                        function: pd.function,
                        expected_type: Type::Bool,
                        value_type: ValueType::RValue,
                    },
                )?;
                let expr_int = expr_value.as_ref().to_int(node.location)?;
                let result = self
                    .ir_builder
                    .builder
                    .build_xor(
                        expr_int,
                        self.ir_builder.context.bool_type().const_int(1, false),
                        "not.result",
                    )
                    .to_comp_res(node.location)?;
                Ok(Box::new(result))
            }
            SingularOperation::Minus => {
                let expr_value = self.translate_with_cast(
                    node.expr.as_ref(),
                    &ExpressionPayload {
                        path: pd.path.clone(),
                        function: pd.function,
                        expected_type: pd.expected_type.clone(),
                        value_type: ValueType::RValue,
                    },
                )?;
                let expr_int = expr_value.as_ref().to_int(node.location)?;
                let result = self
                    .ir_builder
                    .builder
                    .build_int_sub(
                        self.ir_builder.context.i32_type().const_int(0, false),
                        expr_int,
                        "minus.result",
                    )
                    .to_comp_res(node.location)?;
                Ok(Box::new(result))
            }
        }
    }

    fn visit_function_call(
        &self,
        node: &FunctionCall,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if pd.value_type == ValueType::LValue {
            return compiler_err!(node.location, "Error while compiling a call to function \"{}\", its return value is used as an l-value", node.name.value.as_str());
        }

        let path = self
            .ir_builder
            .symbol_table
            .get_identifier_path(&pd.path, &node.name)
            .to_comp_res(node.location)?;

        let symbol = self
            .ir_builder
            .symbol_table
            .find_symbol(&path, &node.name.value)
            .to_comp_res(node.location)?;
        if let Type::Function(fn_type) = &symbol.data_type {
            assert_eq!(fn_type.args.len(), node.args.len());

            let mut call_args = Vec::<BasicMetadataValueEnum>::new();
            self.collect_function_call_args(
                &pd.path,
                &pd.function,
                &fn_type,
                node.args.as_ref(),
                0,
                &mut call_args,
            )?;

            let func = self
                .ir_builder
                .ir_value_storage
                .find_func(&path, &node.name.value)
                .to_comp_res_with_desc(node.location, "no function found.")?;
            let call_res = self
                .ir_builder
                .builder
                .build_call(*func, call_args.as_ref(), node.name.value.as_ref())
                .to_comp_res(node.location)?;
            let res = call_res.try_as_basic_value();

            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(self.ir_builder.null_ptr())
            }
        } else {
            compiler_err!(
                node.location,
                "tried to call an object that's not a function"
            )
        }
    }

    fn visit_get_element(
        &self,
        node: &GetElementNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let obj_type = self.deduce_type(pd.path.clone(), Type::Void, node.object.as_ref())?;

        let ct_index: Option<u64> = if let AnyExpressionNode::Number(num) = node.index.as_ref() {
            Some(num.number)
        } else {
            None
        };

        let value_type =
            if obj_type.is_ptr_type() || (ct_index.is_some() && obj_type.is_static_array()) {
                ValueType::RValue
            } else {
                ValueType::LValue
            };

        let obj_value = self.visit_expression(
            node.object.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: Type::RawPtr,
                value_type,
            },
        )?;

        let index_type = self.deduce_type(
            pd.path.clone(),
            Type::Integer(false, DataSize::Bits32),
            node.index.as_ref(),
        )?;
        let index_value = self.visit_expression(
            node.index.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: index_type,
                value_type: ValueType::RValue,
            },
        )?;
        let index = index_value.as_ref().to_int(node.location)?;

        let element_type = match &obj_type {
            Type::StaticArray(sub_type, _) => sub_type.as_ref().clone(),
            Type::TypedPtr(sub_type) => sub_type.as_ref().clone(),
            _ => pd.expected_type.clone(),
        };

        if let Some(index) = ct_index {
            if pd.value_type == ValueType::RValue && obj_type.is_static_array() {
                return Ok(Box::new(
                    self.ir_builder
                        .builder
                        .build_extract_value(
                            obj_value.as_ref().to_array(node.location)?,
                            index as u32,
                            "extractedval",
                        )
                        .to_comp_res(node.location)?,
                ));
            }
        }

        let obj_ptr = obj_value.as_ref().to_ptr(node.location)?;

        let indexed_ptr = self.build_get_element_ptr(
            node.location,
            &pd.path,
            &element_type,
            obj_ptr,
            &[index],
            "addrindex",
        )?;
        match pd.value_type {
            ValueType::LValue => Ok(Box::<PointerValue<'ctx>>::new(indexed_ptr.into())),
            ValueType::RValue => Ok(Box::<BasicValueEnum<'ctx>>::new(self.load_variable(
                &node.location,
                &pd.path,
                &pd.expected_type,
                &indexed_ptr,
                "addrvalue",
            )?)),
            ValueType::None => Ok(self.ir_builder.null_ptr()),
        }
    }

    fn visit_get_field(
        &self,
        node: &GetFieldNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let obj_type = self.deduce_type(
            pd.path.clone(),
            pd.expected_type.clone(),
            node.object_expr.as_ref(),
        )?;
        let obj = self.visit_expression(
            node.object_expr.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: obj_type.clone(),
                value_type: ValueType::LValue,
            },
        )?;

        let sym = node.get_symbol(
            node.location,
            self.ir_builder.symbol_table,
            &pd.path,
            &obj_type,
        )?;

        if let SymbolType::StructField(id) = sym.sym_type {
            match obj.as_ref().as_basic_value_enum() {
                BasicValueEnum::PointerValue(ptr) => {
                    let field_ptr = self.build_get_element_ptr(
                        node.location,
                        &pd.path,
                        &obj_type,
                        ptr,
                        &[
                            self.ir_builder.context.i32_type().const_int(0, true),
                            self.ir_builder
                                .context
                                .i32_type()
                                .const_int(id as u64, true),
                        ],
                        &node.field_name,
                    )?;
                    match pd.value_type {
                        ValueType::LValue => Ok(Box::new(field_ptr)),
                        ValueType::RValue => Ok(Box::new(self.load_variable(
                            &node.location,
                            &pd.path,
                            &sym.data_type,
                            &field_ptr,
                            "structfield",
                        )?)),
                        _ => compiler_err!(node.location, "invalid symbol"),
                    }
                }
                BasicValueEnum::StructValue(struct_val) => Ok(Box::new(
                    self.ir_builder
                        .builder
                        .build_extract_value(struct_val, id as u32, "extracted")
                        .to_comp_res(node.location)?,
                )),
                _ => compiler_err!(node.location, "invalid symbol"),
            }
        } else {
            compiler_err!(node.location, "invalid symbol")
        }
    }

    fn visit_cast(
        &self,
        node: &CastNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if pd.value_type == ValueType::LValue {
            compiler_err!(node.location, "expression is not R-value");
        }

        let source_type =
            self.deduce_type(pd.path.clone(), pd.expected_type.clone(), &node.expr)?;
        let expr = self.visit_expression(
            node.expr.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: source_type.clone(),
                value_type: ValueType::RValue,
            },
        )?;

        self.build_cast(node.location, &source_type, &node.target_type, expr)
    }

    fn visit_method_call(&self, node: &MethodCall, pd: &Self::Payload) -> Self::VisitResult {
        let obj_type = self.deduce_type(
            pd.path.clone(),
            pd.expected_type.clone(),
            node.object_expr.as_ref(),
        )?;
        let method_path =
            node.get_method_path(self.ir_builder.symbol_table, &pd.path, &obj_type)?;

        let method = self
            .ir_builder
            .symbol_table
            .find_by_path(&method_path)
            .to_comp_res_with_desc(node.location, "unknown method")?;

        if let Type::Function(fn_type) = &method.data_type {
            let self_arg = &fn_type.args[0];

            let receiver = self.visit_expression(
                node.object_expr.as_ref(),
                &ExpressionPayload {
                    path: pd.path.clone(),
                    function: pd.function,
                    expected_type: self_arg.arg_type.clone(),
                    value_type: if self_arg.is_ref {
                        ValueType::LValue
                    } else {
                        ValueType::RValue
                    },
                },
            )?;

            let mut call_args = Vec::<BasicMetadataValueEnum<'ctx>>::new();
            call_args.push(receiver.as_basic_value_enum().into());
            self.collect_function_call_args(
                &pd.path,
                &pd.function,
                &fn_type,
                node.args.as_ref(),
                1,
                &mut call_args,
            )?;

            let func = self
                .ir_builder
                .ir_value_storage
                .find_func(&method_path.parent(), &node.name)
                .to_comp_res_with_desc(node.location, "no function found.")?;
            let call_res = self
                .ir_builder
                .builder
                .build_call(*func, call_args.as_ref(), node.name.as_ref())
                .to_comp_res(node.location)?;

            let res = call_res.try_as_basic_value();
            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(self.ir_builder.null_ptr())
            }
        } else {
            compiler_err!(node.location, "invalid function type")
        }
    }

    fn visit_constructor(&self, node: &ConstructorNode, pd: &Self::Payload) -> Self::VisitResult {
        assert_eq!(pd.value_type, ValueType::RValue);
        self.evaluate_compile_time_value(pd.path.clone(), pd.expected_type.clone(), &node.into())
    }
}
