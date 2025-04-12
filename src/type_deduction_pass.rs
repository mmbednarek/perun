use crate::ast::*;
use crate::error::{CompilerResult, CompilerResultErrorMapper, CompilerResultErrorMapperWithDesc};
use crate::symbols::{SymbolInfo, SymbolPath, SymbolTable};
use crate::typing::Type;

pub struct TypeDeductionPass<'st> {
    symbol_table: &'st SymbolTable,
}

pub struct Payload {
    pub path: SymbolPath,
    pub expected_type: Type,
}

impl<'st> TypeDeductionPass<'st> {
    pub fn new(symbol_table: &'st SymbolTable) -> TypeDeductionPass<'st> {
        TypeDeductionPass { symbol_table }
    }

    pub fn deduce_operand_and_out_type_for_binary_expression(
        &self,
        node: &BinaryExpressionNode,
        pd: &Payload,
    ) -> CompilerResult<(Type, Type)> {
        let left_deduced_type = self.visit_expression(&node.left, pd)?;
        let right_deduced_type = self.visit_expression(&node.right, pd)?;

        if node.operation == BinaryOperation::Assign {
            if left_deduced_type.is_void() {
                Ok((right_deduced_type.clone(), right_deduced_type))
            } else {
                Ok((left_deduced_type.clone(), left_deduced_type))
            }
        } else {
            let wider_type = left_deduced_type.wider_type(&right_deduced_type);
            if node.operation.is_predicate() {
                Ok((wider_type, Type::Bool))
            } else {
                Ok((wider_type.clone(), wider_type))
            }
        }
    }

    fn get_symbol_for_get_field_node(
        &self,
        node: &GetFieldNode,
        pd: &Payload,
    ) -> CompilerResult<&'st SymbolInfo> {
        let obj_type = self.visit_expression(node.object_expr.as_ref(), pd)?;
        if let Type::Alias(alias) = &obj_type {
            let alias_path = self
                .symbol_table
                .find_symbol_path(&pd.path, alias)
                .to_comp_res(node.location)?;
            let symbol = self
                .symbol_table
                .find_symbol(&alias_path, &node.field_name)
                .to_comp_res(node.location)?;
            Ok(symbol)
        } else {
            compiler_err!(node.location, "invalid object type");
        }
    }

    fn get_path_for_method_call(
        &self,
        node: &MethodCall,
        pd: &Payload,
    ) -> CompilerResult<SymbolPath> {
        let obj_type = self.visit_expression(node.object_expr.as_ref(), pd)?;
        if let Type::Alias(alias) = &obj_type {
            let receiver_path = self
                .symbol_table
                .find_symbol_path(&pd.path, alias)
                .to_comp_res(node.location)?;
            Ok(receiver_path.sub(&node.name))
        } else {
            compiler_err!(node.location, "invalid object type")
        }
    }
}

impl<'st> ExpressionVisitor for TypeDeductionPass<'st> {
    type Payload = Payload;
    type VisitResult = CompilerResult<Type>;

    fn visit_identifier(&self, node: &IdentifierNode, pd: &Payload) -> CompilerResult<Type> {
        let symbol = self
            .symbol_table
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;
        Ok(symbol.data_type.clone())
    }

    fn visit_null(&self, _: &NullNode, _: &Payload) -> CompilerResult<Type> {
        Ok(Type::RawPtr)
    }

    fn visit_self(&self, node: &SelfNode, pd: &Payload) -> CompilerResult<Type> {
        let sym = self
            .symbol_table
            .find_symbol(&pd.path, "self")
            .to_comp_res(node.location)?;
        Ok(sym.data_type.clone())
    }

    fn visit_number(&self, node: &NumberNode, pd: &Payload) -> CompilerResult<Type> {
        if pd.expected_type.is_int_type() && !pd.expected_type.is_bool_type() {
            Ok(pd.expected_type.clone())
        } else {
            if u32::try_from(node.number).is_ok() {
                Ok(Type::Int32)
            } else {
                Ok(Type::Int64)
            }
        }
    }

    fn visit_string(&self, _: &StringNode, _: &Payload) -> CompilerResult<Type> {
        Ok(Type::RawPtr)
    }

    fn visit_binary_expression(
        &self,
        node: &BinaryExpressionNode,
        pd: &Payload,
    ) -> CompilerResult<Type> {
        let (_, out_type) = self.deduce_operand_and_out_type_for_binary_expression(node, pd)?;
        Ok(out_type)
    }

    fn visit_singular_expression(
        &self,
        node: &SingularExpressionNode,
        pd: &Payload,
    ) -> CompilerResult<Type> {
        match node.operation {
            SingularOperation::AddressOf => Ok(Type::RawPtr),
            SingularOperation::Deference => Ok(pd.expected_type.clone()),
            SingularOperation::Not => Ok(Type::Bool),
            SingularOperation::Minus => Ok(pd.expected_type.clone()),
        }
    }

    fn visit_function_call(&self, node: &FunctionCall, pd: &Payload) -> CompilerResult<Type> {
        let symbol = self
            .symbol_table
            .find_symbol(&pd.path, &node.name)
            .to_comp_res(node.location)?;
        if let Type::Function(fn_type) = &symbol.data_type {
            return Ok(fn_type.ret_type.clone());
        }

        compiler_err!(
            node.location,
            "tried to call an object that's not a function"
        );
    }

    fn visit_get_element(&self, _: &GetElementNode, pd: &Payload) -> CompilerResult<Type> {
        Ok(pd.expected_type.clone())
    }

    fn visit_get_field(&self, node: &GetFieldNode, pd: &Payload) -> CompilerResult<Type> {
        let sym = self.get_symbol_for_get_field_node(node, pd)?;
        Ok(sym.data_type.clone())
    }

    fn visit_cast(&self, node: &CastNode, _: &Payload) -> CompilerResult<Type> {
        Ok(node.target_type.clone())
    }

    fn visit_method_call(&self, node: &MethodCall, pd: &Payload) -> CompilerResult<Type> {
        let method_path = self.get_path_for_method_call(node, pd)?;
        let method = self
            .symbol_table
            .find_by_path(&method_path)
            .to_comp_res_with_desc(node.location, "unknown method")?;

        if let Type::Function(func_type) = &method.data_type {
            Ok(func_type.ret_type.clone())
        } else {
            compiler_err!(node.location, "invalid receiver type")
        }
    }
}

pub fn deduce_type(
    symbol_table: &SymbolTable,
    path: SymbolPath,
    expected_type: Type,
    node: &AnyExpressionNode,
) -> CompilerResult<Type> {
    let type_deduction_pass = TypeDeductionPass::new(symbol_table);
    type_deduction_pass.visit_expression(
        node.into(),
        &Payload {
            path,
            expected_type,
        },
    )
}
