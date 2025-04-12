use crate::ast::*;
use crate::error::{CompilerResult, CompilerResultErrorMapper, CompilerResultErrorMapperWithDesc};
use crate::ir_build_context::{BasicValueExtension, IRBuildContext};
use crate::module_api;
use crate::symbols::{SymbolPath, SymbolType};
use crate::token::Location;
use crate::type_deduction_pass::{deduce_type, TypeDeductionPass};
use crate::typing::{FuncTypeBox, Type, ValueType};
use either::Either;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::AddressSpace;
use std::fmt::Pointer;

impl BinaryOperation {
    pub fn build<'ctx, 'st>(
        &self,
        gen: &IRBuildContext<'ctx, 'st>,
        location: &Location,
        operand_type: &Type,
        lhs: &dyn BasicValue<'ctx>,
        rhs: &dyn BasicValue<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if *self == BinaryOperation::Assign {
            let lhs_ptr = lhs.to_ptr(*location)?;

            if operand_type.is_int_type() {
                let rhs_int = rhs.to_int(*location)?;
                gen.builder
                    .build_store(lhs_ptr, rhs_int)
                    .to_comp_res(*location)?;
                return Ok(Box::new(lhs_ptr));
            } else if operand_type.is_ptr_type() {
                let rhs_ptr = rhs.to_ptr(*location)?;
                gen.builder
                    .build_store(lhs_ptr, rhs_ptr)
                    .to_comp_res(*location)?;
                return Ok(Box::new(lhs_ptr));
            }

            compiler_err!(*location, "undefined operation");
        }

        if operand_type.is_int_type() {
            let lhs_int = lhs.to_int(*location)?;
            let rhs_int = rhs.to_int(*location)?;

            match self {
                BinaryOperation::Add => {
                    return Ok(Box::new(
                        gen.builder
                            .build_int_add(lhs_int, rhs_int, "sum")
                            .to_comp_res(*location)?,
                    ));
                }
                BinaryOperation::Subtract => {
                    return Ok(Box::new(
                        gen.builder
                            .build_int_sub(lhs_int, rhs_int, "sub")
                            .to_comp_res(*location)?,
                    ));
                }
                BinaryOperation::Divide => {
                    return Ok(Box::new(
                        gen.builder
                            .build_int_signed_div(lhs_int, rhs_int, "div")
                            .to_comp_res(*location)?,
                    ));
                }
                BinaryOperation::Multiply => {
                    return Ok(Box::new(
                        gen.builder
                            .build_int_mul(lhs_int, rhs_int, "mul")
                            .to_comp_res(*location)?,
                    ));
                }
                BinaryOperation::Modulo => {
                    return Ok(Box::new(
                        gen.builder
                            .build_int_signed_rem(lhs_int, rhs_int, "mod")
                            .to_comp_res(*location)?,
                    ));
                }
                binary_op => {
                    if let Some(predicate) = binary_op.to_llvm_int_predicate() {
                        return Ok(Box::new(
                            gen.builder
                                .build_int_compare(predicate, lhs_int, rhs_int, "pred")
                                .to_comp_res(*location)?,
                        ));
                    } else {
                        compiler_err!(*location, "invalid operation");
                    }
                }
            };
        } else if operand_type.is_ptr_type() {
            let lhs_ptr = lhs.to_ptr(*location)?;
            let rhs_ptr = rhs.to_ptr(*location)?;

            if let Some(predicate) = self.to_llvm_int_predicate() {
                return Ok(Box::new(
                    gen.builder
                        .build_int_compare(predicate, lhs_ptr, rhs_ptr, "pred")
                        .to_comp_res(*location)?,
                ));
            } else {
                compiler_err!(*location, "invalid operation");
            }
        }

        compiler_err!(*location, "invalid type");
    }
}

pub struct IRTranslationPass<'irb, 'ctx, 'st> {
    gen: &'irb mut IRBuildContext<'ctx, 'st>,
}

impl<'irb, 'ctx, 'st> IRTranslationPass<'irb, 'ctx, 'st> {
    pub fn new(ir_builder: &'irb mut IRBuildContext<'ctx, 'st>) -> Self {
        Self { gen: ir_builder }
    }

    fn translate_scope(
        &mut self,
        node: &ScopeNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<BasicBlock<'ctx>> {
        let basic_block = self.gen.context.append_basic_block(pd.function, &node.name);
        self.gen.builder.position_at_end(basic_block);

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
        deduce_type(self.gen.symtable, path, expected_type, node)
    }

    fn evaluate_compile_time_value(
        &self,
        path: SymbolPath,
        expected_type: Type,
        node: &AnyExpressionNode,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        crate::compile_time_evaluation_pass::CompileTimeEvaluationPass::new(self.gen)
            .visit_expression(
                node,
                &crate::compile_time_evaluation_pass::Payload {
                    path,
                    expected_type,
                },
            )
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
            let deduced_type = self.deduce_type(pd.path.clone(), pd.expected_type.clone(), node)?;
            self.gen
                .build_cast(*node.get_location(), &deduced_type, &pd.expected_type, stmt)
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
        self.gen
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
        match node.operation {
            BinaryOperation::LogicalAnd => {
                let and_block = self.gen.context.append_basic_block(*function, "and.pred");
                self.translate_to_boolean(
                    node.left.as_ref(),
                    path,
                    function,
                    and_block,
                    false_block,
                )?;
                self.gen.builder.position_at_end(and_block);
                self.translate_to_boolean(
                    node.right.as_ref(),
                    path,
                    function,
                    true_block,
                    false_block,
                )?;
            }
            BinaryOperation::LogicalOr => {
                let or_block = self.gen.context.append_basic_block(*function, "or.pred");
                self.translate_to_boolean(
                    node.left.as_ref(),
                    path,
                    function,
                    true_block,
                    or_block,
                )?;
                self.gen.builder.position_at_end(or_block);
                self.translate_to_boolean(
                    node.right.as_ref(),
                    path,
                    function,
                    true_block,
                    false_block,
                )?;
            }
            _ => {
                self.build_boolean_branch(&node.into(), path, function, true_block, false_block)?;
            }
        };

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
}

fn collect_function_call_args<'irb, 'ctx, 'st>(
    pass: &IRTranslationPass<'irb, 'ctx, 'st>,
    path: &SymbolPath,
    function: &FunctionValue<'ctx>,
    fn_type: &FuncTypeBox<Type>,
    args: &[ExpressionBox],
    arg_offset: usize,
    call_args: &mut Vec<BasicMetadataValueEnum<'ctx>>,
) -> CompilerResult<()> {
    for (i, arg_expr) in args.iter().enumerate() {
        let arg_type = &fn_type.args[i + arg_offset];
        let arg_value = if arg_type.is_ref {
            pass.visit_expression(
                arg_expr.as_ref(),
                &ExpressionPayload {
                    path: path.clone(),
                    function: function.clone(),
                    expected_type: arg_type.arg_type.clone(),
                    value_type: ValueType::LValue,
                },
            )?
        } else {
            pass.visit_expression(
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
            .gen
            .symtable
            .find_by_path(&sym_path)
            .to_comp_res_with_desc(node.location, "internal error")?;
        let basic_value = self.evaluate_compile_time_value(
            path.clone(),
            sym.data_type.clone(),
            node.value.as_ref(),
        )?;
        self.gen
            .addrtable
            .register_basic_value(sym_path, basic_value);
        Ok(())
    }

    fn visit_function(&mut self, node: &FunctionNode, path: &SymbolPath) -> CompilerResult<()> {
        let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for param in &node.params {
            if param.is_ref {
                args.push(self.gen.context.ptr_type(AddressSpace::from(0)).into());
            } else {
                let resolved_type = self
                    .gen
                    .symtable
                    .resolve_type_alias(path, param.arg_type.clone())
                    .to_comp_res(node.location)?;
                visit_type!(
                    param.location,
                    self.gen.context,
                    &resolved_type,
                    value,
                    Ok(args.push(value.into()))
                )?;
            }
        }

        let fn_type = visit_any_type!(
            node.location,
            self.gen.context,
            &node.ret_type,
            value,
            Ok(value.fn_type(&args[..], false))
        )?;

        let linkage = match node.linkage {
            FunctionLinkage::Standard => None,
            FunctionLinkage::External => Some(Linkage::External),
        };

        let sub_path = node.sub_path(path)?;

        let function = self
            .gen
            .module
            .add_function(&node.effective_name()?, fn_type, linkage);
        self.gen.addrtable.register_func(sub_path.clone(), function);

        match &node.scope {
            Some(scope) => {
                let basic_block = self.gen.context.append_basic_block(function, &node.name);
                self.gen.builder.position_at_end(basic_block);

                for (path, sym) in self.gen.symtable.iterate_path(&sub_path) {
                    match sym.sym_type {
                        SymbolType::LocalVariable => {
                            let resolved_type = self
                                .gen
                                .symtable
                                .resolve_type_alias(path, sym.data_type.clone())
                                .to_comp_res(node.location)?;
                            let addr =
                                self.gen
                                    .alloc_var(sym.location, &resolved_type, &sym.name)?;
                            self.gen.addrtable.register_ptr(path.clone(), addr);
                        }
                        SymbolType::LocalReference => {
                            let addr =
                                self.gen.alloc_var(sym.location, &Type::RawPtr, &sym.name)?;
                            self.gen.addrtable.register_ptr(path.clone(), addr);
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

    fn visit_struct(&mut self, _: &StructNode, _: &SymbolPath) -> CompilerResult<()> {
        Ok(())
    }

    fn visit_import(&mut self, node: &ImportNode, path: &SymbolPath) -> CompilerResult<()> {
        let module = module_api::load_module(&format!("{}.json", node.module_name))
            .to_comp_res_with_desc(node.location, "unable to load module")?;

        for func in module.functions {
            let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
            for arg in &func.args {
                if arg.is_ref {
                    args.push(self.gen.context.ptr_type(AddressSpace::from(0)).into());
                } else {
                    let resolved_type = self
                        .gen
                        .symtable
                        .resolve_type_alias(path, Type::from_string(&arg.arg_type))
                        .to_comp_res(node.location)?;
                    visit_type!(
                        node.location,
                        self.gen.context,
                        &resolved_type,
                        value,
                        Ok(args.push(value.into()))
                    )?;
                }
            }

            let ret_type = self
                .gen
                .symtable
                .resolve_type_alias(path, Type::from_string(&func.return_type))
                .to_comp_res(node.location)?;

            let fn_type = visit_any_type!(
                node.location,
                self.gen.context,
                &ret_type,
                value,
                Ok(value.fn_type(&args[..], false))
            )?;

            let function =
                self.gen
                    .module
                    .add_function(&func.name, fn_type, Some(Linkage::External));
            self.gen
                .addrtable
                .register_func(path.sub(&func.name), function);
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
            .gen
            .symtable
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
                    self.gen
                        .builder
                        .build_return(Some(value.as_ref()))
                        .to_comp_res(node.location)?;
                }
                None => {
                    self.gen
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
                .gen
                .symtable
                .find_symbol(&pd.path, &node.name)
                .to_comp_res(node.location)?;
            let addr = *self
                .gen
                .addrtable
                .find_symbol(&pd.path, &node.name)
                .to_comp_res(node.location)?;
            let resolved_type = self
                .gen
                .symtable
                .resolve_type_alias(&pd.path, symbol.data_type.clone())
                .to_comp_res(node.location)?;

            let value = self.translate_with_cast(
                expr,
                &ExpressionPayload {
                    path: pd.path.clone(),
                    function: pd.function,
                    expected_type: resolved_type,
                    value_type: ValueType::RValue,
                },
            )?;

            if symbol.data_type.is_int_type() {
                let int_value = value.as_ref().to_int(node.location)?;
                self.gen
                    .builder
                    .build_store(addr, int_value)
                    .to_comp_res(node.location)?;
            } else if symbol.data_type == Type::RawPtr {
                let ptr_value = value.as_ref().to_ptr(node.location)?;
                self.gen
                    .builder
                    .build_store(addr, ptr_value)
                    .to_comp_res(node.location)?;
            } else {
                compiler_err!(node.location, "unsupported variable type")
            }
        }
        Ok(())
    }

    fn visit_ref_decl_node(
        &mut self,
        node: &RefDeclNode,
        pd: &Self::Payload,
    ) -> CompilerResult<()> {
        let symbol = self
            .gen
            .symtable
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;
        let addr = *self
            .gen
            .addrtable
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;
        let resolved_type = self
            .gen
            .symtable
            .resolve_type_alias(&pd.path, symbol.data_type.clone())
            .to_comp_res(node.location)?;

        let value = self.visit_expression(
            node.expression.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: resolved_type,
                value_type: ValueType::LValue,
            },
        )?;

        let ptr_value = value.as_ref().to_ptr(node.location)?;
        self.gen
            .builder
            .build_store(addr, ptr_value)
            .to_comp_res(node.location)?;

        Ok(())
    }

    fn visit_if_node(&mut self, node: &IfNode, pd: &StatementPayload<'ctx>) -> CompilerResult<()> {
        let current_block = self
            .gen
            .builder
            .get_insert_block()
            .to_comp_res_with_desc(node.location, "statement not located in a valid block")?;

        let ifthen = self.translate_scope(
            &node.then_scope,
            &StatementPayload {
                path: pd.path.sub(&node.then_scope.name),
                function: pd.function,
            },
        )?;
        let ifend = self.gen.context.append_basic_block(pd.function, "if.end");
        self.gen
            .builder
            .build_unconditional_branch(ifend)
            .to_comp_res(node.location)?;

        let ifelse = if let Some(else_scope) = &node.else_scope {
            let scope = self.translate_scope(
                else_scope,
                &StatementPayload {
                    path: pd.path.sub(&else_scope.name),
                    function: pd.function,
                },
            )?;
            self.gen
                .builder
                .build_unconditional_branch(ifend)
                .to_comp_res(node.location)?;
            scope
        } else {
            ifend
        };

        self.gen.builder.position_at_end(current_block);
        self.translate_to_boolean(
            node.condition.as_ref(),
            &pd.path,
            &pd.function,
            ifthen,
            ifelse,
        )?;

        self.gen.builder.position_at_end(ifend);

        Ok(())
    }

    fn visit_while_node(
        &mut self,
        node: &WhileNode,
        pd: &StatementPayload<'ctx>,
    ) -> CompilerResult<()> {
        let while_cond = self
            .gen
            .context
            .append_basic_block(pd.function, "while.cond");
        self.gen
            .builder
            .build_unconditional_branch(while_cond)
            .to_comp_res(node.location)?;
        self.gen.builder.position_at_end(while_cond);

        let while_body = self.translate_scope(
            &node.scope,
            &StatementPayload {
                path: pd.path.sub(&node.scope.name),
                function: pd.function,
            },
        )?;

        self.gen
            .builder
            .build_unconditional_branch(while_cond)
            .to_comp_res(node.location)?;

        let while_end = self
            .gen
            .context
            .append_basic_block(pd.function, "while.end");

        self.gen.builder.position_at_end(while_cond);
        self.translate_to_boolean(
            node.condition.as_ref(),
            &pd.path,
            &pd.function,
            while_body,
            while_end,
        )?;

        self.gen.builder.position_at_end(while_end);

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
        let symbol = self
            .gen
            .symtable
            .find_symbol(&pd.path, &node.name)
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
                        ValueType::RValue => Ok(Box::new(self.gen.load_var(
                            node.location,
                            &symbol.data_type,
                            &arg_ptr,
                            node.name.as_ref(),
                        )?)),
                        ValueType::None => Ok(self.gen.null_ptr()),
                    }
                } else {
                    Ok(Box::new(arg_expr))
                }
            }
            SymbolType::LocalVariable => {
                let (sym, ptr) =
                    self.gen
                        .find_symbol_with_addr(node.location, &pd.path, node.name.as_ref())?;
                let data_type = self
                    .gen
                    .symtable
                    .resolve_type_alias(&pd.path, sym.data_type.clone())
                    .to_comp_res(node.location)?;
                match pd.value_type {
                    ValueType::LValue => Ok(Box::new(*ptr)),
                    ValueType::RValue => Ok(Box::new(self.gen.load_var(
                        node.location,
                        &data_type,
                        ptr,
                        node.name.as_ref(),
                    )?)),
                    ValueType::None => Ok(self.gen.null_ptr()),
                }
            }
            SymbolType::LocalReference => {
                let (sym, ptr) =
                    self.gen
                        .find_symbol_with_addr(node.location, &pd.path, node.name.as_ref())?;
                let loaded_ptr_var =
                    self.gen
                        .load_var(node.location, &Type::RawPtr, ptr, node.name.as_ref())?;
                let loaded_ptr = loaded_ptr_var.to_ptr(node.location)?;
                match pd.value_type {
                    ValueType::LValue => Ok(Box::new(loaded_ptr)),
                    ValueType::RValue => Ok(Box::new(self.gen.load_var(
                        node.location,
                        &sym.data_type,
                        &loaded_ptr,
                        node.name.as_ref(),
                    )?)),
                    ValueType::None => Ok(self.gen.null_ptr()),
                }
            }
            SymbolType::ConstantDef => {
                let basic_val = self
                    .gen
                    .addrtable
                    .find_basic_value(&pd.path, &node.name)
                    .to_comp_res_with_desc(node.location, "unable to find value")?;
                Ok(Box::new(basic_val.as_basic_value_enum()))
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
        Ok(self.gen.null_ptr())
    }

    fn visit_self(
        &self,
        node: &SelfNode,
        pd: &ExpressionPayload<'ctx>,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let sym = self
            .gen
            .symtable
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
        if pd.value_type == ValueType::LValue {
            return compiler_err!(node.location, "tried to interpret a number as an l-value");
        }

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

        let arr = self
            .gen
            .context
            .i8_type()
            .array_type(node.value.len() as u32);
        let global_val = self.gen.module.add_global(arr, None, "str");
        global_val.set_constant(true);

        let str_val = self.gen.context.const_string(node.value.as_bytes(), true);
        global_val.set_initializer(&str_val);

        Ok(Box::new(global_val.as_pointer_value()))
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

        match node.operation {
            BinaryOperation::LogicalAnd => {
                let and_block = self.gen.context.append_basic_block(pd.function, "and.pred");
                let end_block = self.gen.context.append_basic_block(pd.function, "end.pred");

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
                    .gen
                    .builder
                    .get_insert_block()
                    .to_comp_res_with_desc(node.location, "invalid block")?;
                self.gen
                    .builder
                    .build_conditional_branch(left_int, and_block, end_block)
                    .to_comp_res(*node.get_location())?;

                self.gen.builder.position_at_end(and_block);

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
                    .gen
                    .builder
                    .get_insert_block()
                    .to_comp_res_with_desc(node.location, "invalid block")?;
                self.gen
                    .builder
                    .build_unconditional_branch(end_block)
                    .to_comp_res(node.location)?;

                self.gen.builder.position_at_end(end_block);

                let result = self
                    .gen
                    .builder
                    .build_phi(self.gen.context.bool_type(), "and.result")
                    .to_comp_res(node.location)?;
                result.add_incoming(&[
                    (&self.gen.context.bool_type().const_zero(), left_block),
                    (right.as_ref(), right_block),
                ]);

                return Ok(Box::new(result.as_basic_value()));
            }
            BinaryOperation::LogicalOr => {
                let or_block = self.gen.context.append_basic_block(pd.function, "or.pred");
                let end_block = self.gen.context.append_basic_block(pd.function, "end.pred");

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
                    .gen
                    .builder
                    .get_insert_block()
                    .to_comp_res_with_desc(node.location, "invalid block")?;
                self.gen
                    .builder
                    .build_conditional_branch(left_int, end_block, or_block)
                    .to_comp_res(*node.get_location())?;

                self.gen.builder.position_at_end(or_block);

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
                    .gen
                    .builder
                    .get_insert_block()
                    .to_comp_res_with_desc(node.location, "invalid block")?;
                self.gen
                    .builder
                    .build_unconditional_branch(end_block)
                    .to_comp_res(node.location)?;

                self.gen.builder.position_at_end(end_block);

                let result = self
                    .gen
                    .builder
                    .build_phi(self.gen.context.bool_type(), "or.result")
                    .to_comp_res(node.location)?;
                result.add_incoming(&[
                    (
                        &self.gen.context.bool_type().const_int(1, false),
                        left_block,
                    ),
                    (right.as_ref(), right_block),
                ]);

                return Ok(Box::new(result.as_basic_value()));
            }
            _ => {}
        };

        // For assigment we always expect the left hand side to be an l-value.
        let left_value_type = node.get_left_value_type();

        let (operand_type, _) = TypeDeductionPass::new(self.gen.symtable)
            .deduce_operand_and_out_type_for_binary_expression(
                node,
                &crate::type_deduction_pass::Payload {
                    path: pd.path.clone(),
                    expected_type: pd.expected_type.clone(),
                },
            )?;

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

        node.operation.build(
            self.gen,
            &node.location,
            &operand_type,
            left.as_ref(),
            right.as_ref(),
        )
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
                        Ok(Box::new(self.gen.load_var(
                            node.location,
                            &pd.expected_type,
                            &ptr,
                            "deref",
                        )?))
                    }
                    ValueType::None => Ok(self.gen.null_ptr()),
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
                    .gen
                    .builder
                    .build_xor(
                        expr_int,
                        self.gen.context.bool_type().const_int(1, false),
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
                    .gen
                    .builder
                    .build_int_sub(
                        self.gen.context.i32_type().const_int(0, false),
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
            return compiler_err!(
                node.location,
                "tried to interpret function call as an l-value"
            );
        }

        let symbol = self
            .gen
            .symtable
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;
        if let Type::Function(fn_type) = &symbol.data_type {
            if fn_type.args.len() != node.args.len() {
                return compiler_err!(node.location, "invalid number of arguments");
            }

            let mut call_args = Vec::<BasicMetadataValueEnum>::new();
            collect_function_call_args(
                self,
                &pd.path,
                &pd.function,
                &fn_type,
                node.args.as_ref(),
                0,
                &mut call_args,
            )?;

            let func = self
                .gen
                .addrtable
                .find_func(&pd.path, &node.name)
                .to_comp_res_with_desc(node.location, "no function found.")?;
            let call_res = self
                .gen
                .builder
                .build_call(*func, call_args.as_ref(), node.name.as_ref())
                .to_comp_res(node.location)?;
            let res = call_res.try_as_basic_value();

            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(self.gen.null_ptr())
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
        let obj_value = self.visit_expression(
            node.object.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: Type::RawPtr,
                value_type: ValueType::RValue,
            },
        )?;
        let obj_ptr = obj_value.as_ref().to_ptr(node.location)?;
        let index_type = self.deduce_type(
            pd.path.clone(),
            pd.expected_type.clone(),
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

        let indexed_ptr = self.gen.build_get_element_ptr(
            node.location,
            &pd.expected_type,
            obj_ptr,
            &[index],
            "addrindex",
        )?;
        match pd.value_type {
            ValueType::LValue => Ok(Box::<PointerValue<'ctx>>::new(indexed_ptr.into())),
            ValueType::RValue => Ok(Box::<BasicValueEnum<'ctx>>::new(self.gen.load_var(
                node.location,
                &pd.expected_type,
                &indexed_ptr,
                "addrvalue",
            )?)),
            ValueType::None => Ok(self.gen.null_ptr()),
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
        let obj_resolved = self
            .gen
            .symtable
            .resolve_type_alias(&pd.path, obj_type.clone())
            .to_comp_res(node.location)?;
        let obj = self.visit_expression(
            node.object_expr.as_ref(),
            &ExpressionPayload {
                path: pd.path.clone(),
                function: pd.function,
                expected_type: obj_resolved.clone(),
                value_type: ValueType::LValue,
            },
        )?;

        let sym = node.get_symbol(self.gen.symtable, &pd.path, &obj_type)?;

        if let SymbolType::StructField(id) = sym.sym_type {
            match obj.as_ref().as_basic_value_enum() {
                BasicValueEnum::PointerValue(ptr) => {
                    let field_ptr = self.gen.build_get_element_ptr(
                        node.location,
                        &obj_resolved,
                        ptr,
                        &[
                            self.gen.context.i32_type().const_int(0, true),
                            self.gen.context.i32_type().const_int(id as u64, true),
                        ],
                        &node.field_name,
                    )?;
                    match pd.value_type {
                        ValueType::LValue => Ok(Box::new(field_ptr)),
                        ValueType::RValue => Ok(Box::new(self.gen.load_var(
                            node.location,
                            &sym.data_type,
                            &field_ptr,
                            "structfield",
                        )?)),
                        _ => compiler_err!(node.location, "invalid symbol"),
                    }
                }
                BasicValueEnum::StructValue(struct_val) => Ok(Box::new(
                    self.gen
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

        self.gen
            .build_cast(node.location, &source_type, &node.target_type, expr)
    }

    fn visit_method_call(&self, node: &MethodCall, pd: &Self::Payload) -> Self::VisitResult {
        let obj_type = self.deduce_type(
            pd.path.clone(),
            pd.expected_type.clone(),
            node.object_expr.as_ref(),
        )?;
        let method_path = node.get_method_path(self.gen.symtable, &pd.path, &obj_type)?;

        let method = self
            .gen
            .symtable
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
            collect_function_call_args(
                self,
                &pd.path,
                &pd.function,
                &fn_type,
                node.args.as_ref(),
                1,
                &mut call_args,
            )?;

            let func = self
                .gen
                .addrtable
                .find_func(&method_path.parent(), &node.name)
                .to_comp_res_with_desc(node.location, "no function found.")?;
            let call_res = self
                .gen
                .builder
                .build_call(*func, call_args.as_ref(), node.name.as_ref())
                .to_comp_res(node.location)?;

            let res = call_res.try_as_basic_value();
            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(self.gen.null_ptr())
            }
        } else {
            compiler_err!(node.location, "invalid function type")
        }
    }
}
