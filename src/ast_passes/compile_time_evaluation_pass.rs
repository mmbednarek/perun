use crate::ast::*;
use crate::ast_passes::ir_translation_pass::BasicValueBox;
use crate::ast_passes::type_deduction_pass::deduce_type;
use crate::error::{CompilerResult, CompilerResultErrorMapperWithDesc};
use crate::ir_build_context::IRBuildContext;
use crate::symbols::SymbolPath;
use crate::typing::Type;
use inkwell::types::AnyTypeEnum;

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

        let llvm_type = num_type
            .to_llvm_type(self.build_context.context)
            .to_comp_res_with_desc(node.location, "unable to map llvm type")?;

        if let AnyTypeEnum::IntType(it) = llvm_type {
            Ok(Box::new(it.const_int(node.number, true)))
        } else {
            compiler_err!(node.location, "tried assigning number to a non int type");
        }
    }

    fn visit_string(&self, node: &StringNode, _: &Self::Payload) -> Self::VisitResult {
        compiler_err!(node.location, "string cannot be evaluated at compile time")
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
}
