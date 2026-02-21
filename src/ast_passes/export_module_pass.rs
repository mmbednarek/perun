use crate::ast::*;
use crate::error::CompilerResultErrorMapperWithDesc;
use crate::module::Module;

pub struct ExportModulePass<'a> {
    pub module: Module,
    pub import_directories: &'a [String],
}

impl<'id> ExportModulePass<'id> {
    pub fn new(module_name: String, import_directories: &'id [String]) -> Self {
        Self {
            module: Module {
                name: module_name,
                functions: vec![],
                constants: vec![],
                structs: vec![],
                enums: vec![],
                aliases: vec![],
                unions: vec![],
            },
            import_directories,
        }
    }
}

impl GlobalStatementVisitor for ExportModulePass<'_> {
    type Payload = ();
    type VisitResult = ();

    fn visit_source_unit(&mut self, node: &SourceUnit, _: &Self::Payload) -> Self::VisitResult {
        for sub_node in &node.body {
            self.visit_global_statement(sub_node.as_ref(), &());
        }
    }

    fn visit_const_decl(&mut self, node: &ConstDeclNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            self.module.constants.push(node.clone());
        }
    }

    fn visit_function(&mut self, node: &FunctionNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            self.module.functions.push(node.clone());
        }
    }

    fn visit_struct(&mut self, node: &StructNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            self.module.structs.push(node.clone());
        }
    }

    fn visit_import(&mut self, node: &ImportNode, _: &Self::Payload) -> Self::VisitResult {
        if !node.is_public {
            return;
        }

        let mut opt_module: Option<Module> = None;
        for path in self.import_directories {
            opt_module = Module::new(
                &format!("{}/{}.json", path.as_str(), node.module_name).as_str(),
                node.location,
            );
            if opt_module.is_some() {
                break;
            }
        }
        let module = opt_module.unwrap();

        for alias in module.aliases {
            self.module.aliases.push(alias);
        }
        for constant in module.constants {
            self.module.constants.push(constant);
        }
        for func in module.functions {
            self.module.functions.push(func);
        }
        for struct_node in module.structs {
            self.module.structs.push(struct_node);
        }
        for enum_node in module.enums {
            self.module.enums.push(enum_node);
        }
        for union_node in module.unions {
            self.module.unions.push(union_node);
        }
    }

    fn visit_enum(&mut self, node: &EnumNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            self.module.enums.push(node.clone());
        }
    }

    fn visit_alias(&mut self, node: &AliasNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            self.module.aliases.push(node.clone());
        }
    }

    fn visit_union(&mut self, node: &UnionNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            self.module.unions.push(node.clone());
        }
    }

    fn visit_module(&mut self, node: &ModuleNode, _: &Self::Payload) -> Self::VisitResult {
        self.module.name = node.name.to_string();
    }
}
