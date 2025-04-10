use crate::ast::*;
use crate::error::{
    wrap_option, CompilerError, CompilerResult, CompilerResultErrorMapper,
    CompilerResultErrorMapperWithDesc,
};
use crate::ir_build_context::{BasicValueExtension, IRBuildContext};
use crate::symbols::{SymbolInfo, SymbolPath, SymbolTable, SymbolType};
use crate::token::Location;
use crate::typing::{FuncType, FuncTypeArg, FuncTypeBox, StructType, Type, ValueType};
use crate::{module_api, visit_any_type, visit_type};
use either::Either;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use inkwell::{AddressSpace, IntPredicate};

impl BinaryOperation {
    pub fn build<'ctx, 'st>(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
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

fn create_method_name(receiver: &str, name: &str) -> String {
    format!("perun.method.{}.{}", receiver, name)
}

// **********************************
// ******** GLOBAL STATEMENTS *******
// **********************************

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for SourceUnit<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
    ) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.generate(gen, path)?;
        }
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.collect_symbols(path, symtable)?;
        }
        Ok(())
    }
}

impl<'ctx, 'st> ScopeNode<'ctx, 'st> {
    fn generate_il(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<BasicBlock<'ctx>> {
        let basic_block = gen.context.append_basic_block(*function, &self.name);
        gen.builder.position_at_end(basic_block);

        self.generate_to_current_block(gen, path, function)?;

        Ok(basic_block)
    }

    fn generate_to_current_block(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.generate(gen, path, function)?;
        }
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        for stmt in &self.body {
            stmt.collect_symbols(path, symtable)?;
        }
        Ok(())
    }
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for ConstDeclNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
    ) -> CompilerResult<()> {
        let sym_path = path.sub(&self.name);
        let sym = wrap_option(
            self.location,
            gen.symtable.find_by_path(&sym_path),
            "internal error",
        )?;
        let basic_val = self.value.to_constexpr_value(gen, path, &sym.data_type)?;
        gen.addrtable.register_basic_value(sym_path, basic_val);
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let expected_type = match self.const_type.clone() {
            Some(t) => t,
            None => self.value.deduce_type(symtable, path, &Type::Void)?,
        };
        symtable
            .add_symbol(
                path,
                SymbolInfo {
                    name: self.name.clone(),
                    sym_type: SymbolType::ConstantDef,
                    data_type: expected_type,
                    location: self.location,
                },
            )
            .to_comp_res(self.location)?;
        Ok(())
    }
}

impl<'ctx, 'st> FunctionNode<'ctx, 'st> {
    fn sub_path(&self, path: &SymbolPath) -> CompilerResult<SymbolPath> {
        if let Some(self_type) = &self.self_type {
            match self_type {
                Type::Alias(alias) => Ok(path.sub(alias).sub(&self.name)),
                _ => compiler_err!(self.location, "invalid type"),
            }
        } else {
            Ok(path.sub(&self.name))
        }
    }

    fn effective_name(&self) -> CompilerResult<String> {
        if let Some(self_type) = &self.self_type {
            match self_type {
                Type::Alias(alias) => Ok(create_method_name(alias, &self.name)),
                _ => compiler_err!(self.location, "invalid type"),
            }
        } else {
            Ok(self.name.clone())
        }
    }
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for FunctionNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
    ) -> CompilerResult<()> {
        let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
        for param in &self.params {
            if param.is_ref {
                args.push(gen.context.ptr_type(AddressSpace::from(0)).into());
            } else {
                let resolved_type = gen
                    .symtable
                    .resolve_type_alias(path, param.arg_type.clone())
                    .to_comp_res(self.location)?;
                visit_type!(
                    param.location,
                    gen.context,
                    &resolved_type,
                    value,
                    Ok(args.push(value.into()))
                )?;
            }
        }

        let fn_type = visit_any_type!(
            self.location,
            gen.context,
            &self.ret_type,
            value,
            Ok(value.fn_type(&args[..], false))
        )?;

        let linkage = match self.linkage {
            FunctionLinkage::Standard => None,
            FunctionLinkage::External => Some(Linkage::External),
        };

        let sub_path = self.sub_path(path)?;

        let function = gen
            .module
            .add_function(&self.effective_name()?, fn_type, linkage);
        gen.addrtable.register_func(sub_path.clone(), function);

        match &self.scope {
            Some(scope) => {
                let basic_block = gen.context.append_basic_block(function, &self.name);
                gen.builder.position_at_end(basic_block);

                for (path, sym) in gen.symtable.iterate_path(&sub_path) {
                    match sym.sym_type {
                        SymbolType::LocalVariable => {
                            let resolved_type = gen
                                .symtable
                                .resolve_type_alias(path, sym.data_type.clone())
                                .to_comp_res(self.location)?;
                            let addr = gen.alloc_var(sym.location, &resolved_type, &sym.name)?;
                            gen.addrtable.register_ptr(path.clone(), addr);
                        }
                        SymbolType::LocalReference => {
                            let addr = gen.alloc_var(sym.location, &Type::RawPtr, &sym.name)?;
                            gen.addrtable.register_ptr(path.clone(), addr);
                        }
                        _ => {}
                    }
                }

                scope.generate_to_current_block(gen, &sub_path, &function)?;
            }
            None => {}
        }

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let mut types = Vec::<FuncTypeArg<Type>>::new();
        let sub_path = self.sub_path(path)?;

        for (i, param) in self.params.iter().enumerate() {
            types.push(FuncTypeArg {
                is_ref: param.is_ref,
                arg_type: param.arg_type.clone(),
            });
            symtable
                .add_symbol(
                    &sub_path,
                    SymbolInfo {
                        name: param.name.to_string(),
                        sym_type: SymbolType::FunctionArg(i, param.is_ref),
                        data_type: param.arg_type.clone(),
                        location: self.location,
                    },
                )
                .to_comp_res(self.location)?;
        }
        symtable
            .add_symbol(
                &sub_path.parent(),
                SymbolInfo {
                    name: self.name.to_string(),
                    sym_type: SymbolType::FunctionDef,
                    data_type: Type::Function(Box::new(FuncType {
                        args: types,
                        ret_type: self.ret_type.clone(),
                    })),
                    location: self.location,
                },
            )
            .to_comp_res(self.location)?;

        match &self.scope {
            Some(scope) => {
                scope.collect_symbols(&sub_path, symtable)?;
            }
            None => {}
        }

        Ok(())
    }
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for StructNode {
    fn generate(&self, _: &mut IRBuildContext<'ctx, 'st>, _: &SymbolPath) -> CompilerResult<()> {
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        for (i, field) in self.fields.iter().enumerate() {
            let subpath = path.sub(&self.name);
            symtable
                .add_symbol(
                    &subpath,
                    SymbolInfo {
                        name: field.name.clone(),
                        sym_type: SymbolType::StructField(i),
                        data_type: field.field_type.clone(),
                        location: field.location,
                    },
                )
                .to_comp_res(self.location)?;
        }

        let fields: Vec<Type> = self
            .fields
            .iter()
            .map(|field| field.field_type.clone())
            .collect();
        let struct_type = StructType { fields };
        let symbol = SymbolInfo {
            name: self.name.clone(),
            sym_type: SymbolType::TypeDef,
            data_type: Type::Struct(Box::new(struct_type)),
            location: self.location,
        };
        symtable
            .add_symbol(path, symbol)
            .to_comp_res(self.location)?;
        Ok(())
    }
}

impl<'ctx, 'st> GlobalStatementNode<'ctx, 'st> for ImportNode {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
    ) -> CompilerResult<()> {
        let module = module_api::load_module(&format!("{}.json", self.module_name))
            .to_comp_res_with_desc(self.location, "unable to load module")?;

        for func in module.functions {
            let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
            for arg in &func.args {
                if arg.is_ref {
                    args.push(gen.context.ptr_type(AddressSpace::from(0)).into());
                } else {
                    let resolved_type = gen
                        .symtable
                        .resolve_type_alias(path, Type::from_string(&arg.arg_type))
                        .to_comp_res(self.location)?;
                    visit_type!(
                        self.location,
                        gen.context,
                        &resolved_type,
                        value,
                        Ok(args.push(value.into()))
                    )?;
                }
            }

            let ret_type = gen
                .symtable
                .resolve_type_alias(path, Type::from_string(&func.return_type))
                .to_comp_res(self.location)?;

            let fn_type = visit_any_type!(
                self.location,
                gen.context,
                &ret_type,
                value,
                Ok(value.fn_type(&args[..], false))
            )?;

            let function =
                gen.module
                    .add_function(&func.name, fn_type, Some(Linkage::External));
            gen.addrtable.register_func(path.sub(&func.name), function);
        }
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let module = module_api::load_module(&format!("{}.json", self.module_name))
            .to_comp_res_with_desc(self.location, "unable to load module")?;

        for func in module.functions {
            let mut args: Vec<FuncTypeArg<Type>> = Vec::new();
            for arg in &func.args {
                args.push(FuncTypeArg {
                    is_ref: arg.is_ref,
                    arg_type: Type::from_string(&arg.arg_type),
                });
            }

            let ret_type = symtable
                .resolve_type_alias(path, Type::from_string(&func.return_type))
                .to_comp_res(self.location)?;

            symtable
                .add_symbol(
                    path,
                    SymbolInfo {
                        name: func.name.to_string(),
                        sym_type: SymbolType::FunctionDef,
                        data_type: Type::Function(Box::new(FuncType {
                            args,
                            ret_type,
                        })),
                        location: self.location,
                    },
                )
                .to_comp_res(self.location)?;
        }

        Ok(())
    }
}

// **********************************
// ********** STATEMENTS ************
// **********************************

impl<'ctx, 'st> StatementNode<'ctx, 'st> for ReturnNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        let msg = format!("unable to find function type {}", path);
        let func_symbol =
            wrap_option(self.location, gen.symtable.find_by_path(path), msg.as_ref())?;
        if let Type::Function(func_type) = &func_symbol.data_type {
            match &self.expression {
                Some(expr) => {
                    let value = expr.generate_casted(
                        gen,
                        path,
                        function,
                        &func_type.ret_type,
                        &ValueType::RValue,
                    )?;
                    gen.builder
                        .build_return(Some(value.as_ref()))
                        .to_comp_res(self.location)?;
                }
                None => {
                    gen.builder.build_return(None).to_comp_res(self.location)?;
                }
            }
            return Ok(());
        }

        compiler_err!(self.location, "invalid type");
    }

    fn collect_symbols(
        &self,
        _path: &SymbolPath,
        _symtable: &mut SymbolTable,
    ) -> CompilerResult<()> {
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::ReturnNode(self)
    }
}

impl<'ctx, 'st> VarDeclNode<'ctx, 'st> {
    fn deduce_type(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<Type> {
        if let Some(vt) = &self.var_type {
            Ok(vt.clone())
        } else {
            match &self.expression {
                Some(expr) => expr.deduce_type(&symtable, path, &Type::Void),
                None => Ok(Type::Void),
            }
        }
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for VarDeclNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        if let Some(expr) = &self.expression {
            let symbol = gen
                .symtable
                .find_symbol(path, &self.name)
                .to_comp_res(self.location)?;
            let addr = *gen
                .addrtable
                .find_symbol(path, &self.name)
                .to_comp_res(self.location)?;
            let resolved_type = gen
                .symtable
                .resolve_type_alias(path, symbol.data_type.clone())
                .to_comp_res(self.location)?;

            let value =
                expr.generate_casted(gen, path, function, &resolved_type, &ValueType::RValue)?;
            if symbol.data_type.is_int_type() {
                let int_value = value.as_ref().to_int(self.location)?;
                gen.builder
                    .build_store(addr, int_value)
                    .to_comp_res(self.location)?;
            } else if symbol.data_type == Type::RawPtr {
                let ptr_value = value.as_ref().to_ptr(self.location)?;
                gen.builder
                    .build_store(addr, ptr_value)
                    .to_comp_res(self.location)?;
            } else {
                compiler_err!(self.location, "unsupported variable type")
            }
        }
        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let var_type = self.deduce_type(path, symtable)?;
        symtable
            .add_symbol(
                path,
                SymbolInfo {
                    name: self.name.clone(),
                    sym_type: SymbolType::LocalVariable,
                    data_type: var_type,
                    location: self.location,
                },
            )
            .to_comp_res(self.location)?;
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::VarDeclNode(self)
    }
}

impl<'ctx, 'st> RefDeclNode<'ctx, 'st> {
    fn deduce_type(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<Type> {
        if let Some(vt) = &self.var_type {
            Ok(vt.clone())
        } else {
            self.expression.deduce_type(&symtable, path, &Type::Void)
        }
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for RefDeclNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        let symbol = gen
            .symtable
            .find_symbol(path, &self.name)
            .to_comp_res(self.location)?;
        let addr = *gen
            .addrtable
            .find_symbol(path, &self.name)
            .to_comp_res(self.location)?;
        let resolved_type = gen
            .symtable
            .resolve_type_alias(path, symbol.data_type.clone())
            .to_comp_res(self.location)?;

        let value =
            self.expression
                .generate(gen, path, function, &resolved_type, &ValueType::LValue)?;
        let ptr_value = value.as_ref().to_ptr(self.location)?;
        gen.builder
            .build_store(addr, ptr_value)
            .to_comp_res(self.location)?;

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        let var_type = self.deduce_type(path, symtable)?;
        symtable
            .add_symbol(
                path,
                SymbolInfo {
                    name: self.name.clone(),
                    sym_type: SymbolType::LocalReference,
                    data_type: var_type,
                    location: self.location,
                },
            )
            .to_comp_res(self.location)?;
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::RefDeclNode(self)
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for IfNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        let current_block = wrap_option(
            self.location,
            gen.builder.get_insert_block(),
            "statement not located in a valid block",
        )?;

        let ifthen =
            self.then_scope
                .generate_il(gen, &path.sub(&self.then_scope.name), function)?;
        let ifend = gen.context.append_basic_block(*function, "if.end");
        gen.builder
            .build_unconditional_branch(ifend)
            .to_comp_res(self.location)?;

        let ifelse = if let Some(else_scope) = &self.else_scope {
            let scope = else_scope.generate_il(gen, &path.sub(&else_scope.name), function)?;
            gen.builder
                .build_unconditional_branch(ifend)
                .to_comp_res(self.location)?;
            scope
        } else {
            ifend
        };

        gen.builder.position_at_end(current_block);
        self.condition
            .generate_boolean(gen, path, function, ifthen, ifelse)?;

        gen.builder.position_at_end(ifend);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        self.then_scope
            .collect_symbols(&path.sub(&self.then_scope.name), symtable)?;
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::IfNode(self)
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for WhileNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        let while_cond = gen.context.append_basic_block(*function, "while.cond");
        gen.builder
            .build_unconditional_branch(while_cond)
            .to_comp_res(self.location)?;
        gen.builder.position_at_end(while_cond);

        let while_body = self
            .scope
            .generate_il(gen, &path.sub(&self.scope.name), function)?;
        gen.builder
            .build_unconditional_branch(while_cond)
            .to_comp_res(self.location)?;

        let while_end = gen.context.append_basic_block(*function, "while.end");

        gen.builder.position_at_end(while_cond);
        self.condition
            .generate_boolean(gen, path, function, while_body, while_end)?;

        gen.builder.position_at_end(while_end);

        Ok(())
    }

    fn collect_symbols(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<()> {
        self.scope
            .collect_symbols(&path.sub(&self.scope.name), symtable)?;
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::WhileNode(self)
    }
}

impl<'ctx, 'st> StatementNode<'ctx, 'st> for ExpressionStatementNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
    ) -> CompilerResult<()> {
        self.expression
            .generate(gen, path, function, &Type::Void, &ValueType::None)?;
        Ok(())
    }

    fn collect_symbols(
        &self,
        _path: &SymbolPath,
        _symtable: &mut SymbolTable,
    ) -> CompilerResult<()> {
        Ok(())
    }

    fn to_any_statement_node<'stmt>(&'stmt self) -> AnyStatementNode<'stmt, 'ctx, 'st> {
        AnyStatementNode::ExpressionStatementNode(self)
    }
}

// **********************************
// ********** EXPRESSIONS ***********
// **********************************

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for IdentifierNode {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        _: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let symbol = gen
            .symtable
            .find_symbol(path, &self.name)
            .to_comp_res(self.location)?;
        match symbol.sym_type {
            SymbolType::FunctionArg(index, is_ref) => {
                let arg_expr = wrap_option(
                    symbol.location,
                    function.get_nth_param(index as u32),
                    "invalid function argument",
                )?;
                if is_ref {
                    let arg_ptr = arg_expr.to_ptr(symbol.location)?;
                    match *value_type {
                        ValueType::LValue => Ok(Box::new(arg_ptr)),
                        ValueType::RValue => Ok(Box::new(gen.load_var(
                            self.location,
                            &symbol.data_type,
                            &arg_ptr,
                            self.name.as_ref(),
                        )?)),
                        ValueType::None => Ok(gen.null_ptr()),
                    }
                } else {
                    Ok(Box::new(arg_expr))
                }
            }
            SymbolType::LocalVariable => {
                let (sym, ptr) =
                    gen.find_symbol_with_addr(self.location, path, self.name.as_ref())?;
                let data_type = gen
                    .symtable
                    .resolve_type_alias(path, sym.data_type.clone())
                    .to_comp_res(self.location)?;
                match *value_type {
                    ValueType::LValue => Ok(Box::new(*ptr)),
                    ValueType::RValue => Ok(Box::new(gen.load_var(
                        self.location,
                        &data_type,
                        ptr,
                        self.name.as_ref(),
                    )?)),
                    ValueType::None => Ok(gen.null_ptr()),
                }
            }
            SymbolType::LocalReference => {
                let (sym, ptr) =
                    gen.find_symbol_with_addr(self.location, path, self.name.as_ref())?;
                let loaded_ptr_var =
                    gen.load_var(self.location, &Type::RawPtr, ptr, self.name.as_ref())?;
                let loaded_ptr = loaded_ptr_var.to_ptr(self.location)?;
                match *value_type {
                    ValueType::LValue => Ok(Box::new(loaded_ptr)),
                    ValueType::RValue => Ok(Box::new(gen.load_var(
                        self.location,
                        &sym.data_type,
                        &loaded_ptr,
                        self.name.as_ref(),
                    )?)),
                    ValueType::None => Ok(gen.null_ptr()),
                }
            }
            SymbolType::ConstantDef => {
                let basic_val = wrap_option(
                    self.location,
                    gen.addrtable.find_basic_value(path, &self.name),
                    "unable to find value",
                )?;
                Ok(Box::new(basic_val.as_basic_value_enum()))
            }
            _ => {
                compiler_err!(self.location, "TODO: Implement")
            }
        }
    }

    fn deduce_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        _: &Type,
    ) -> CompilerResult<Type> {
        let symbol = symtable
            .find_symbol(path, &self.name)
            .to_comp_res(self.location)?;
        Ok(symbol.data_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for NullNode {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        _: &SymbolPath,
        _: &FunctionValue<'ctx>,
        _: &Type,
        _: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        Ok(gen.null_ptr())
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(Type::RawPtr)
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for SelfNode {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        func: &FunctionValue<'ctx>,
        _: &Type,
        _: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let sym = gen
            .symtable
            .find_symbol(path, "self")
            .to_comp_res(self.location)?;
        if let SymbolType::FunctionArg(index, _) = sym.sym_type {
            let arg_expr = wrap_option(
                sym.location,
                func.get_nth_param(index as u32),
                "invalid function argument",
            )?;
            Ok(Box::new(arg_expr))
        } else {
            compiler_err!(self.location, "self is defined incorrectly");
        }
    }

    fn deduce_type(&self, tb: &SymbolTable, path: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        let sym = tb.find_symbol(path, "self").to_comp_res(self.location)?;
        Ok(sym.data_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for NumberNode {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        _function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret a number as an l-value");
        }

        self.to_constexpr_value(gen, path, expected_type)
    }

    fn deduce_type(
        &self,
        _: &SymbolTable,
        _: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<Type> {
        if expected_type.is_int_type() && !expected_type.is_bool_type() {
            Ok(expected_type.clone())
        } else {
            if u32::try_from(self.number).is_ok() {
                Ok(Type::Int32)
            } else {
                Ok(Type::Int64)
            }
        }
    }

    fn get_location(&self) -> &Location {
        &self.location
    }

    fn to_constexpr_value(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let num_type = self.deduce_type(&gen.symtable, path, expected_type)?;
        let llvm_type = wrap_option(
            self.location,
            num_type.to_llvm_type(gen.context),
            "unable to map llvm type",
        )?;
        if let AnyTypeEnum::IntType(it) = llvm_type {
            Ok(Box::new(it.const_int(self.number, true)))
        } else {
            compiler_err!(self.location, "tried assigning number to a non int type");
        }
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for StringNode {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        _: &SymbolPath,
        _function: &FunctionValue<'ctx>,
        _: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "tried to interpret a string as an l-value");
        }

        let arr = gen.context.i8_type().array_type(self.value.len() as u32);
        let global_val = gen.module.add_global(arr, None, "str");
        global_val.set_constant(true);

        let str_val = gen.context.const_string(self.value.as_bytes(), true);
        global_val.set_initializer(&str_val);

        Ok(Box::new(global_val.as_pointer_value()))
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(Type::RawPtr)
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> BinaryExpressionNode<'ctx, 'st> {
    fn deduce_left_value_type(&self) -> ValueType {
        match self.operation {
            BinaryOperation::Assign => ValueType::LValue,
            _ => ValueType::RValue,
        }
    }

    fn deduce_operand_and_out_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<(Type, Type)> {
        let left_deduced_type = self.left.deduce_type(symtable, path, expected_type)?;
        let right_deduced_type = self.right.deduce_type(symtable, path, expected_type)?;

        if self.operation == BinaryOperation::Assign {
            if left_deduced_type.is_void() {
                Ok((right_deduced_type.clone(), right_deduced_type))
            } else {
                Ok((left_deduced_type.clone(), left_deduced_type))
            }
        } else {
            let wider_type = left_deduced_type.wider_type(&right_deduced_type);
            if self.operation.is_predicate() {
                Ok((wider_type, Type::Bool))
            } else {
                Ok((wider_type.clone(), wider_type))
            }
        }
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for BinaryExpressionNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(
                self.location,
                "tried to interpret assignment expression as an l-value"
            );
        }

        match self.operation {
            BinaryOperation::LogicalAnd => {
                let and_block = gen.context.append_basic_block(*function, "and.pred");
                let end_block = gen.context.append_basic_block(*function, "end.pred");

                let left = self.left.generate_casted(
                    gen,
                    path,
                    function,
                    &Type::Bool,
                    &ValueType::RValue,
                )?;
                let left_int = left.as_ref().to_int(*self.get_location())?;
                let left_block = wrap_option(
                    self.location,
                    gen.builder.get_insert_block(),
                    "invalid block",
                )?;
                gen.builder
                    .build_conditional_branch(left_int, and_block, end_block)
                    .to_comp_res(*self.get_location())?;

                gen.builder.position_at_end(and_block);

                let right = self.right.generate_casted(
                    gen,
                    path,
                    function,
                    &Type::Bool,
                    &ValueType::RValue,
                )?;
                let right_block = wrap_option(
                    self.location,
                    gen.builder.get_insert_block(),
                    "invalid block",
                )?;
                gen.builder
                    .build_unconditional_branch(end_block)
                    .to_comp_res(self.location)?;

                gen.builder.position_at_end(end_block);

                let result = gen
                    .builder
                    .build_phi(gen.context.bool_type(), "and.result")
                    .to_comp_res(self.location)?;
                result.add_incoming(&[
                    (&gen.context.bool_type().const_zero(), left_block),
                    (right.as_ref(), right_block),
                ]);

                return Ok(Box::new(result.as_basic_value()));
            }
            BinaryOperation::LogicalOr => {
                let or_block = gen.context.append_basic_block(*function, "or.pred");
                let end_block = gen.context.append_basic_block(*function, "end.pred");

                let left = self.left.generate_casted(
                    gen,
                    path,
                    function,
                    &Type::Bool,
                    &ValueType::RValue,
                )?;
                let left_int = left.as_ref().to_int(*self.get_location())?;
                let left_block = wrap_option(
                    self.location,
                    gen.builder.get_insert_block(),
                    "invalid block",
                )?;
                gen.builder
                    .build_conditional_branch(left_int, end_block, or_block)
                    .to_comp_res(*self.get_location())?;

                gen.builder.position_at_end(or_block);

                let right = self.right.generate_casted(
                    gen,
                    path,
                    function,
                    &Type::Bool,
                    &ValueType::RValue,
                )?;
                let right_block = wrap_option(
                    self.location,
                    gen.builder.get_insert_block(),
                    "invalid block",
                )?;
                gen.builder
                    .build_unconditional_branch(end_block)
                    .to_comp_res(self.location)?;

                gen.builder.position_at_end(end_block);

                let result = gen
                    .builder
                    .build_phi(gen.context.bool_type(), "or.result")
                    .to_comp_res(self.location)?;
                result.add_incoming(&[
                    (&gen.context.bool_type().const_int(1, false), left_block),
                    (right.as_ref(), right_block),
                ]);

                return Ok(Box::new(result.as_basic_value()));
            }
            _ => {}
        };

        // For assigment we always expect the left hand side to be an l-value.
        let left_value_type = self.deduce_left_value_type();
        let (operand_type, _) =
            self.deduce_operand_and_out_type(&gen.symtable, path, expected_type)?;

        let left =
            self.left
                .generate_casted(gen, path, function, &operand_type, &left_value_type)?;
        let right =
            self.right
                .generate_casted(gen, path, function, &operand_type, &ValueType::RValue)?;

        self.operation.build(
            gen,
            &self.location,
            &operand_type,
            left.as_ref(),
            right.as_ref(),
        )
    }

    fn generate_boolean(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        match self.operation {
            BinaryOperation::LogicalAnd => {
                let and_block = gen.context.append_basic_block(*function, "and.pred");
                self.left
                    .generate_boolean(gen, path, function, and_block, false_block)?;
                gen.builder.position_at_end(and_block);
                self.right
                    .generate_boolean(gen, path, function, true_block, false_block)?;
            }
            BinaryOperation::LogicalOr => {
                let or_block = gen.context.append_basic_block(*function, "or.pred");
                self.left
                    .generate_boolean(gen, path, function, true_block, or_block)?;
                gen.builder.position_at_end(or_block);
                self.right
                    .generate_boolean(gen, path, function, true_block, false_block)?;
            }
            _ => {
                self.build_boolean_branch(gen, path, function, true_block, false_block)?;
            }
        };

        Ok(())
    }

    fn deduce_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<Type> {
        let (_, out_type) = self.deduce_operand_and_out_type(symtable, path, expected_type)?;
        Ok(out_type)
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for SingularExpressionNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        match self.operation {
            SingularOperation::AddressOf => {
                if *value_type == ValueType::LValue {
                    compiler_err!(
                        self.location,
                        "tried to interpret assignment expression as an l-value"
                    );
                }
                self.expr
                    .generate(r#gen, path, function, expected_type, &ValueType::LValue)
            }
            SingularOperation::Deference => {
                let ptr_box =
                    self.expr
                        .generate(r#gen, path, function, &Type::RawPtr, &ValueType::RValue)?;
                match value_type {
                    ValueType::LValue => Ok(ptr_box),
                    ValueType::RValue => {
                        let ptr = ptr_box.as_ref().to_ptr(self.location)?;
                        Ok(Box::new(gen.load_var(
                            self.location,
                            expected_type,
                            &ptr,
                            "deref",
                        )?))
                    }
                    ValueType::None => Ok(gen.null_ptr()),
                }
            }
            SingularOperation::Not => {
                let expr_value = self.expr.generate_casted(
                    gen,
                    path,
                    function,
                    &Type::Bool,
                    &ValueType::RValue,
                )?;
                let expr_int = expr_value.as_ref().to_int(self.location)?;
                let result = gen
                    .builder
                    .build_xor(
                        expr_int,
                        gen.context.bool_type().const_int(1, false),
                        "not.result",
                    )
                    .to_comp_res(self.location)?;
                Ok(Box::new(result))
            }
            SingularOperation::Minus => {
                let expr_value = self.expr.generate_casted(
                    gen,
                    path,
                    function,
                    expected_type,
                    &ValueType::RValue,
                )?;
                let expr_int = expr_value.as_ref().to_int(self.location)?;
                let result = gen
                    .builder
                    .build_int_sub(
                        gen.context.i32_type().const_int(0, false),
                        expr_int,
                        "minus.result",
                    )
                    .to_comp_res(self.location)?;
                Ok(Box::new(result))
            }
        }
    }

    fn generate_boolean(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        true_block: BasicBlock<'ctx>,
        false_block: BasicBlock<'ctx>,
    ) -> CompilerResult<()> {
        match self.operation {
            SingularOperation::Not => {
                self.expr
                    .generate_boolean(gen, path, function, false_block, true_block)?;
            }
            _ => {
                self.build_boolean_branch(gen, path, function, true_block, false_block)?;
            }
        };

        Ok(())
    }

    fn deduce_type(
        &self,
        _: &SymbolTable,
        _: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<Type> {
        match self.operation {
            SingularOperation::AddressOf => Ok(Type::RawPtr),
            SingularOperation::Deference => Ok(expected_type.clone()),
            SingularOperation::Not => Ok(Type::Bool),
            SingularOperation::Minus => Ok(expected_type.clone()),
        }
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

fn collect_function_call_args<'ctx, 'st>(
    gen: &mut IRBuildContext<'ctx, 'st>,
    path: &SymbolPath,
    function: &FunctionValue<'ctx>,
    fn_type: &FuncTypeBox<Type>,
    args: &[ExpressionBox<'ctx, 'st>],
    arg_offset: usize,
    call_args: &mut Vec<BasicMetadataValueEnum<'ctx>>,
) -> CompilerResult<()> {
    for (i, arg_expr) in args.iter().enumerate() {
        let arg_type = &fn_type.args[i + arg_offset];
        let arg_value = if arg_type.is_ref {
            arg_expr.generate(gen, path, function, &arg_type.arg_type, &ValueType::LValue)?
        } else {
            arg_expr.generate(gen, path, function, &arg_type.arg_type, &ValueType::RValue)?
        };

        let arg_value_enum = arg_value.as_basic_value_enum();
        call_args.push(arg_value_enum.into());
    }
    Ok(())
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for FunctionCall<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        _expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(
                self.location,
                "tried to interpret function call as an l-value"
            );
        }

        let symbol = gen
            .symtable
            .find_symbol(path, &self.name)
            .to_comp_res(self.location)?;
        if let Type::Function(fn_type) = &symbol.data_type {
            if fn_type.args.len() != self.args.len() {
                compiler_err!(self.location, "invalid number of arguments");
            }

            let mut call_args = Vec::<BasicMetadataValueEnum>::new();
            collect_function_call_args(
                gen,
                path,
                function,
                &fn_type,
                self.args.as_ref(),
                0,
                &mut call_args,
            )?;

            let func = gen
                .addrtable
                .find_func(path, &self.name)
                .to_comp_res_with_desc(self.location, "no function found.")?;
            let call_res = gen
                .builder
                .build_call(*func, call_args.as_ref(), self.name.as_ref())
                .to_comp_res(self.location)?;
            let res = call_res.try_as_basic_value();

            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(gen.null_ptr())
            }
        } else {
            compiler_err!(
                self.location,
                "tried to call an object that's not a function"
            );
        }
    }

    fn deduce_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        _: &Type,
    ) -> CompilerResult<Type> {
        let symbol = symtable
            .find_symbol(path, &self.name)
            .to_comp_res(self.location)?;
        if let Type::Function(fn_type) = &symbol.data_type {
            return Ok(fn_type.ret_type.clone());
        }

        compiler_err!(
            self.location,
            "tried to call an object that's not a function"
        );
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for GetElementNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let obj_value =
            self.object
                .generate(gen, path, function, &Type::RawPtr, &ValueType::RValue)?;
        let obj_ptr = obj_value.as_ref().to_ptr(self.location)?;
        let index_type = self.index.deduce_type(&gen.symtable, path, expected_type)?;
        let index_value =
            self.index
                .generate(gen, path, function, &index_type, &ValueType::RValue)?;
        let index = index_value.as_ref().to_int(self.location)?;

        let indexed_ptr = gen.build_get_element_ptr(
            self.location,
            expected_type,
            obj_ptr,
            &[index],
            "addrindex",
        )?;
        match value_type {
            ValueType::LValue => Ok(Box::new(indexed_ptr)),
            ValueType::RValue => Ok(Box::new(gen.load_var(
                self.location,
                expected_type,
                &indexed_ptr,
                "addrvalue",
            )?)),
            ValueType::None => Ok(gen.null_ptr()),
        }
    }

    fn deduce_type(
        &self,
        _: &SymbolTable,
        _: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<Type> {
        Ok(expected_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for CastNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        if *value_type == ValueType::LValue {
            compiler_err!(self.location, "expression is not R-value");
        }

        let source_type = self.expr.deduce_type(&gen.symtable, path, expected_type)?;
        let expr = self
            .expr
            .generate(gen, path, function, &source_type, &ValueType::RValue)?;

        gen.build_cast(self.location, &source_type, &self.target_type, expr)
    }

    fn deduce_type(&self, _: &SymbolTable, _: &SymbolPath, _: &Type) -> CompilerResult<Type> {
        Ok(self.target_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> GetFieldNode<'ctx, 'st> {
    fn get_symbol<'a>(
        &self,
        symtable: &'a SymbolTable,
        path: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<&'a SymbolInfo> {
        let obj_type = self
            .object_expr
            .deduce_type(symtable, path, expected_type)?;
        if let Type::Alias(alias) = &obj_type {
            let alias_path = symtable
                .find_symbol_path(path, alias)
                .to_comp_res(self.location)?;
            let symbol = symtable
                .find_symbol(&alias_path, &self.field_name)
                .to_comp_res(self.location)?;
            Ok(symbol)
        } else {
            compiler_err!(self.location, "invalid object type");
        }
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for GetFieldNode<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let obj_type = self
            .object_expr
            .deduce_type(gen.symtable, path, expected_type)?;
        let obj_resolved = gen
            .symtable
            .resolve_type_alias(path, obj_type)
            .to_comp_res(self.location)?;
        let obj =
            self.object_expr
                .generate(gen, path, function, &obj_resolved, &ValueType::LValue)?;

        let sym = self.get_symbol(gen.symtable, path, expected_type)?;

        if let SymbolType::StructField(id) = sym.sym_type {
            match obj.as_ref().as_basic_value_enum() {
                BasicValueEnum::PointerValue(ptr) => {
                    let field_ptr = gen.build_get_element_ptr(
                        self.location,
                        &obj_resolved,
                        ptr,
                        &[
                            gen.context.i32_type().const_int(0, true),
                            gen.context.i32_type().const_int(id as u64, true),
                        ],
                        &self.field_name,
                    )?;
                    match value_type {
                        ValueType::LValue => Ok(Box::new(field_ptr)),
                        ValueType::RValue => Ok(Box::new(gen.load_var(
                            self.location,
                            &sym.data_type,
                            &field_ptr,
                            "structfield",
                        )?)),
                        _ => compiler_err!(self.location, "invalid symbol"),
                    }
                }
                BasicValueEnum::StructValue(struct_val) => Ok(Box::new(
                    gen.builder
                        .build_extract_value(struct_val, id as u32, "extracted")
                        .to_comp_res(self.location)?,
                )),
                _ => compiler_err!(self.location, "invalid symbol"),
            }
        } else {
            compiler_err!(self.location, "invalid symbol");
        }
    }

    fn deduce_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        expected_type: &Type,
    ) -> CompilerResult<Type> {
        let sym = self.get_symbol(symtable, path, expected_type)?;
        Ok(sym.data_type.clone())
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}

impl<'ctx, 'st> MethodCall<'ctx, 'st> {
    fn get_method_path(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
    ) -> CompilerResult<SymbolPath> {
        let obj_type = self.object_expr.deduce_type(symtable, path, &Type::Void)?;
        if let Type::Alias(alias) = &obj_type {
            let receiver_path = symtable
                .find_symbol_path(path, alias)
                .to_comp_res(self.location)?;
            Ok(receiver_path.sub(&self.name))
        } else {
            compiler_err!(self.location, "invalid object type")
        }
    }
}

impl<'ctx, 'st> ExpressionNode<'ctx, 'st> for MethodCall<'ctx, 'st> {
    fn generate(
        &self,
        gen: &mut IRBuildContext<'ctx, 'st>,
        path: &SymbolPath,
        function: &FunctionValue<'ctx>,
        expected_type: &Type,
        value_type: &ValueType,
    ) -> CompilerResult<BasicValueBox<'ctx>> {
        let method_path = self.get_method_path(gen.symtable, path)?;

        let method = gen
            .symtable
            .find_by_path(&method_path)
            .to_comp_res_with_desc(self.location, "unknown method")?;

        if let Type::Function(fn_type) = &method.data_type {
            let self_arg = &fn_type.args[0];

            let receiver = self.object_expr.generate(
                gen,
                path,
                function,
                &self_arg.arg_type,
                if self_arg.is_ref {
                    &ValueType::LValue
                } else {
                    &ValueType::RValue
                },
            )?;

            let mut call_args = Vec::<BasicMetadataValueEnum<'ctx>>::new();
            call_args.push(receiver.as_basic_value_enum().into());
            collect_function_call_args(
                gen,
                path,
                function,
                &fn_type,
                self.args.as_ref(),
                1,
                &mut call_args,
            )?;

            let func = gen
                .addrtable
                .find_func(&method_path.parent(), &self.name)
                .to_comp_res_with_desc(self.location, "no function found.")?;
            let call_res = gen
                .builder
                .build_call(*func, call_args.as_ref(), self.name.as_ref())
                .to_comp_res(self.location)?;

            let res = call_res.try_as_basic_value();
            if let Either::Left(call_res_bv) = res {
                Ok(Box::new(call_res_bv))
            } else {
                Ok(gen.null_ptr())
            }
        } else {
            compiler_err!(self.location, "invalid function type")
        }
    }

    fn deduce_type(
        &self,
        symtable: &SymbolTable,
        path: &SymbolPath,
        _: &Type,
    ) -> CompilerResult<Type> {
        let method_path = self.get_method_path(symtable, path)?;
        let method = symtable
            .find_by_path(&method_path)
            .to_comp_res_with_desc(self.location, "unknown method")?;

        if let Type::Function(func_type) = &method.data_type {
            Ok(func_type.ret_type.clone())
        } else {
            compiler_err!(self.location, "invalid receiver type")
        }
    }

    fn get_location(&self) -> &Location {
        &self.location
    }
}
