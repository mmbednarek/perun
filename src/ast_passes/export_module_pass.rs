use crate::ast::*;
use crate::module::Module;

pub struct ExportModulePass {
    pub module: Module,
}

impl ExportModulePass {
    pub fn new(module_name: String) -> Self {
        Self {
            module: Module {
                name: module_name,
                functions: vec![],
                constants: vec![],
                structs: vec![],
            },
        }
    }
}

impl GlobalStatementVisitor for ExportModulePass {
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

    fn visit_import(&mut self, _: &ImportNode, _: &Self::Payload) -> Self::VisitResult {}

    fn visit_enum(&mut self, node: &EnumNode, _: &Self::Payload) -> Self::VisitResult {
        if node.is_public {
            todo!()
        }
    }
}
