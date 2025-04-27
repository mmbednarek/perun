use crate::ast::*;
use crate::ast_passes::ir_translation_pass::BasicValueBox;
use crate::ast_passes::type_deduction_pass::deduce_type;
use crate::error::{CompilerResult, CompilerResultErrorMapperWithDesc};
use crate::ir_build_context::{BasicValueExtension, IRBuildContext};
use crate::symbols::SymbolPath;
use crate::typing::{DataSize, Type};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{FloatValue, IntValue, PointerValue};

pub struct CompileTimeEvaluationPass<'irb, 'ctx, 'st> {
    build_context: &'irb IRBuildContext<'ctx, 'st>,
}

impl<'irb, 'ctx, 'st> CompileTimeEvaluationPass<'irb, 'ctx, 'st> {
    pub fn new(build_context: &'irb IRBuildContext<'ctx, 'st>) -> Self {
        Self { build_context }
    }
}

pub struct Payload {
    pub path: SymbolPath,
    pub expected_type: Type,
}

impl<'irb, 'ctx, 'st> ExpressionVisitor for CompileTimeEvaluationPass<'irb, 'ctx, 'st> {
    type Payload = Payload;
    type VisitResult = CompilerResult<BasicValueBox<'ctx>>;

    fn visit_identifier(&self, node: &IdentifierNode, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "identifier cannot be evaluated at compile time"
        )
    }

    fn visit_null(&self, _: &NullNode, _: &Self::Payload) -> Self::VisitResult {
        Ok(self.build_context.null_ptr())
    }

    fn visit_boolean(&self, node: &BooleanNode, _: &Self::Payload) -> Self::VisitResult {
        Ok(Box::new(
            self.build_context
                .context
                .bool_type()
                .const_int(node.value as u64, false),
        ))
    }

    fn visit_self(&self, node: &SelfNode, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(node.location, "self cannot be evaluated at compile time")
    }

    fn visit_number(&self, node: &NumberNode, pd: &Self::Payload) -> Self::VisitResult {
        let num_type = deduce_type(
            &self.build_context.symbol_table,
            pd.path.clone(),
            pd.expected_type.clone(),
            &node.into(),
        )?;

        if let Type::Integer(is_signed, data_size) = num_type {
            Ok(Box::new(
                self.build_context
                    .context
                    .custom_width_int_type(data_size.bit_count())
                    .const_int(node.number, is_signed),
            ))
        } else {
            compiler_err!(node.location, "tried assigning number to a non int type");
        }
    }

    fn visit_floating_point(
        &self,
        node: &FloatingPointNode,
        pd: &Self::Payload,
    ) -> Self::VisitResult {
        let num_type = deduce_type(
            &self.build_context.symbol_table,
            pd.path.clone(),
            pd.expected_type.clone(),
            &node.into(),
        )?;

        if let Type::FloatingPoint(data_size) = num_type {
            Ok(Box::new(
                match data_size {
                    DataSize::Bits16 => Ok(self.build_context.context.f16_type()),
                    DataSize::Bits32 => Ok(self.build_context.context.f32_type()),
                    DataSize::Bits64 => Ok(self.build_context.context.f64_type()),
                    _ => compiler_err!(node.location, "unsupported float size"),
                }?
                .const_float(node.value),
            ))
        } else {
            compiler_err!(node.location, "tried assigning number to a non int type");
        }
    }

    fn visit_string(&self, node: &StringNode, _: &Self::Payload) -> Self::VisitResult {
        let arr = self
            .build_context
            .context
            .i8_type()
            .array_type(node.value.len() as u32);
        let global_val = self.build_context.module.add_global(arr, None, "str");
        global_val.set_constant(true);

        let str_val = self
            .build_context
            .context
            .const_string(node.value.as_bytes(), true);
        global_val.set_initializer(&str_val);

        Ok(Box::new(global_val.as_pointer_value()))
    }

    fn visit_binary_expression(
        &self,
        node: &BinaryExpressionNode,
        _: &Self::Payload,
    ) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "binary expression cannot be evaluated at compile time"
        )
    }

    fn visit_singular_expression(
        &self,
        node: &SingularExpressionNode,
        _: &Self::Payload,
    ) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "singular operation cannot be evaluated at compile time"
        )
    }

    fn visit_function_call(&self, node: &FunctionCall, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "function call cannot be evaluated at compile time"
        )
    }

    fn visit_get_element(&self, node: &GetElementNode, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "get element cannot be evaluated at compile time"
        )
    }

    fn visit_get_field(&self, node: &GetFieldNode, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "get field cannot be evaluated at compile time"
        )
    }

    fn visit_cast(&self, node: &CastNode, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(node.location, "cast cannot be evaluated at compile time")
    }

    fn visit_method_call(&self, node: &MethodCall, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(
            node.location,
            "method call cannot be evaluated at compile time"
        )
    }

    fn visit_constructor(&self, node: &ConstructorNode, pd: &Self::Payload) -> Self::VisitResult {
        match &pd.expected_type {
            Type::StaticArray(sub_type, count) => {
                if (*count) != (node.arguments.len() as u32) {
                    return compiler_err!(
                        node.location,
                        "invalid number of constructor arguments, expected {} args",
                        *count
                    );
                }

                let llvm_type = sub_type
                    .to_llvm_basic_type(self.build_context.context)
                    .to_comp_res_with_desc(node.location, "cannot map to llvm_type")?;

                let array_type = match &llvm_type {
                    BasicTypeEnum::FloatType(ft) => {
                        let mut values = Vec::<FloatValue<'ctx>>::new();
                        for arg in &node.arguments {
                            values.push(
                                self.visit_expression(arg, pd)?
                                    .to_float(*arg.get_location())?,
                            );
                        }
                        ft.const_array(values.as_slice())
                    }
                    BasicTypeEnum::IntType(it) => {
                        let mut values = Vec::<IntValue<'ctx>>::new();
                        for arg in &node.arguments {
                            values.push(
                                self.visit_expression(arg, pd)?
                                    .to_int(*arg.get_location())?,
                            );
                        }
                        it.const_array(values.as_slice())
                    }
                    BasicTypeEnum::PointerType(pt) => {
                        let mut values = Vec::<PointerValue<'ctx>>::new();
                        for arg in &node.arguments {
                            values.push(
                                self.visit_expression(arg, pd)?
                                    .to_ptr(*arg.get_location())?,
                            );
                        }
                        pt.const_array(values.as_slice())
                    }
                    _ => compiler_err!(node.location, "unsupported type"),
                };

                Ok(Box::new(array_type))
            }
            _ => compiler_err!(node.location, "unsupported type"),
        }
    }
}
