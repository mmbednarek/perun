use crate::ast::*;
use crate::ast_passes::type_deduction_pass::deduce_type;
use crate::error::{CompilerResult, CompilerResultErrorMapper, CompilerResultErrorMapperWithDesc};
use crate::module::Module;
use crate::symbols::{SymbolInfo, SymbolPath, SymbolTable, SymbolType};
use crate::typing::{FuncType, FuncTypeArg, StructType, Type};

pub struct CollectSymbolsPass<'st> {
    symbol_table: &'st mut SymbolTable,
    import_directory: String,
}

impl<'st> CollectSymbolsPass<'st> {
    pub fn new(symbol_table: &'st mut SymbolTable, import_directory: String) -> Self {
        CollectSymbolsPass {
            symbol_table,
            import_directory,
        }
    }

    fn visit_scope(&mut self, node: &ScopeNode, path: &SymbolPath) -> CompilerResult<()> {
        for stmt in &node.body {
            self.visit_statement(stmt.as_ref(), path)?;
        }
        Ok(())
    }
}

impl<'st> GlobalStatementVisitor for CollectSymbolsPass<'st> {
    type Payload = SymbolPath;
    type VisitResult = CompilerResult<()>;

    fn visit_source_unit(&mut self, node: &SourceUnit, path: &SymbolPath) -> CompilerResult<()> {
        for stmt in &node.body {
            self.visit_global_statement(stmt, path)?;
        }
        Ok(())
    }

    fn visit_const_decl(&mut self, node: &ConstDeclNode, path: &SymbolPath) -> CompilerResult<()> {
        let expected_type = match node.const_type.clone() {
            Some(t) => t,
            None => deduce_type(
                self.symbol_table,
                path.clone(),
                Type::Void,
                node.value.as_ref().into(),
            )?,
        };
        self.symbol_table
            .add_symbol(
                path,
                SymbolInfo {
                    name: node.name.clone(),
                    sym_type: SymbolType::ConstantDef,
                    data_type: expected_type,
                    location: node.location,
                },
            )
            .to_comp_res(node.location)?;
        Ok(())
    }

    fn visit_function(&mut self, node: &FunctionNode, path: &SymbolPath) -> CompilerResult<()> {
        let mut types = Vec::<FuncTypeArg>::new();
        let sub_path = node.sub_path(node.location, self.symbol_table, path)?;

        for (i, param) in node.params.iter().enumerate() {
            types.push(FuncTypeArg {
                is_ref: param.is_ref,
                arg_type: param.arg_type.clone(),
            });
            self.symbol_table
                .add_symbol(
                    &sub_path,
                    SymbolInfo {
                        name: param.name.to_string(),
                        sym_type: SymbolType::FunctionArg(i, param.is_ref),
                        data_type: param.arg_type.clone(),
                        location: node.location,
                    },
                )
                .to_comp_res(node.location)?;
        }
        self.symbol_table
            .add_symbol(
                &sub_path.parent(),
                SymbolInfo {
                    name: node.name.to_string(),
                    sym_type: SymbolType::FunctionDef,
                    data_type: Type::Function(Box::new(FuncType {
                        args: types,
                        ret_type: node.ret_type.clone(),
                    })),
                    location: node.location,
                },
            )
            .to_comp_res(node.location)?;

        if let Some(scope) = &node.scope {
            self.visit_scope(scope, &sub_path)?;
        }

        Ok(())
    }

    fn visit_struct(&mut self, node: &StructNode, path: &SymbolPath) -> CompilerResult<()> {
        for (i, field) in node.fields.iter().enumerate() {
            let sub_path = path.sub(&node.name);
            self.symbol_table
                .add_symbol(
                    &sub_path,
                    SymbolInfo {
                        name: field.name.clone(),
                        sym_type: SymbolType::StructField(i),
                        data_type: field.field_type.clone(),
                        location: field.location,
                    },
                )
                .to_comp_res(node.location)?;
        }

        let fields: Vec<Type> = node
            .fields
            .iter()
            .map(|field| field.field_type.clone())
            .collect();
        let struct_type = StructType { fields };
        let symbol = SymbolInfo {
            name: node.name.clone(),
            sym_type: SymbolType::TypeDef,
            data_type: Type::Struct(Box::new(struct_type)),
            location: node.location,
        };
        self.symbol_table
            .add_symbol(path, symbol)
            .to_comp_res(node.location)?;
        Ok(())
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

        self.symbol_table
            .add_symbol(
                &SymbolPath::empty(),
                SymbolInfo {
                    name: node.module_name.clone(),
                    sym_type: SymbolType::Namespace,
                    data_type: Type::Void,
                    location: node.location,
                },
            )
            .to_comp_res(node.location)?;

        for struct_node in &module.structs {
            self.visit_struct(struct_node, &module_path)?
        }
        for constant_node in &module.constants {
            self.visit_const_decl(constant_node, &module_path)?
        }
        for func_node in &module.functions {
            self.visit_function(func_node, &module_path)?;
        }

        Ok(())
    }
}

impl VarDeclNode {
    fn deduce_type(&self, path: &SymbolPath, symtable: &mut SymbolTable) -> CompilerResult<Type> {
        if let Some(vt) = &self.var_type {
            Ok(vt.clone())
        } else {
            match &self.expression {
                Some(expr) => deduce_type(symtable, path.clone(), Type::Void, expr),
                None => Ok(Type::Void),
            }
        }
    }
}

impl<'st> StatementVisitor for CollectSymbolsPass<'st> {
    type Payload = SymbolPath;
    type VisitResult = CompilerResult<()>;

    fn visit_return_node(&mut self, _: &ReturnNode, _: &SymbolPath) -> CompilerResult<()> {
        Ok(())
    }

    fn visit_var_decl_node(&mut self, node: &VarDeclNode, path: &SymbolPath) -> CompilerResult<()> {
        let var_type = node.deduce_type(path, self.symbol_table)?;
        self.symbol_table
            .add_symbol(
                path,
                SymbolInfo {
                    name: node.name.clone(),
                    sym_type: SymbolType::LocalVariable,
                    data_type: var_type,
                    location: node.location,
                },
            )
            .to_comp_res(node.location)?;
        Ok(())
    }

    fn visit_ref_decl_node(&mut self, node: &RefDeclNode, path: &SymbolPath) -> CompilerResult<()> {
        let var_type = node.var_type.clone().unwrap_or(deduce_type(
            self.symbol_table,
            path.clone(),
            Type::Void,
            node.expression.as_ref().into(),
        )?);
        self.symbol_table
            .add_symbol(
                path,
                SymbolInfo {
                    name: node.name.clone(),
                    sym_type: SymbolType::LocalReference,
                    data_type: var_type,
                    location: node.location,
                },
            )
            .to_comp_res(node.location)?;
        Ok(())
    }

    fn visit_if_node(&mut self, node: &IfNode, path: &SymbolPath) -> CompilerResult<()> {
        self.visit_scope(&node.then_scope, &path.sub(&node.then_scope.name))?;
        Ok(())
    }

    fn visit_while_node(&mut self, node: &WhileNode, path: &SymbolPath) -> CompilerResult<()> {
        self.visit_scope(&node.scope, &path.sub(&node.scope.name))?;
        Ok(())
    }

    fn visit_expression_statement_node(
        &mut self,
        _: &ExpressionStatementNode,
        _: &SymbolPath,
    ) -> CompilerResult<()> {
        Ok(())
    }
}
