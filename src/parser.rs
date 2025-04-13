use rand::{distributions::Alphanumeric, Rng};

use crate::ast::*;
use crate::error::{wrap_option, CompilerResult};
use crate::token::{Keyword, Location, OperatorType, TokenType};
use crate::token_reader::TokenReader;
use crate::typing::Type;
use std::mem::take;

fn get_random_identifier(prefix: &str) -> String {
    let random_str: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(7)
        .map(char::from)
        .collect();
    format!("{}_{}", prefix, random_str)
}

struct ExpressionBuilder {
    expr_stack: Vec<Option<Box<AnyExpressionNode>>>,
    op_stack: Vec<AnyOperation>,
    last_is_op: bool,
    location: Location,
}

impl ExpressionBuilder {
    fn new(location: Location) -> Self {
        Self {
            expr_stack: Vec::new(),
            op_stack: Vec::new(),
            last_is_op: true,
            location,
        }
    }

    fn push_expr(&mut self, node: AnyExpressionNode) {
        self.expr_stack.push(Some(Box::new(node)));
        self.last_is_op = false;
    }

    fn push_expr_box(&mut self, node: ExpressionBox) {
        self.expr_stack.push(Some(node));
        self.last_is_op = false;
    }

    fn push_op(&mut self, op_type: &OperatorType) -> CompilerResult<()> {
        let op_res = AnyOperation::from_op_type(*op_type, self.last_is_op);
        if let None = op_res {
            compiler_err!(self.location, "failed to read operator");
        }
        let op = op_res.unwrap();

        let mut last_op = self.last_op();
        if !self.last_is_op {
            while last_op.proceeds(&op) {
                self.process_operator()?;
                last_op = self.last_op();
            }
        }
        self.op_stack.push(op);
        self.last_is_op = true;
        Ok(())
    }

    fn last_op(&self) -> AnyOperation {
        *self.op_stack.last().unwrap_or(&AnyOperation::None)
    }

    fn process_operator(&mut self) -> CompilerResult<()> {
        let op_res = self.op_stack.pop();
        if op_res.is_none() {
            compiler_err!(self.location, "no operators left");
        }

        let op = op_res.unwrap();
        match op {
            AnyOperation::Singular(sin_op) => {
                let expr_opt = self.expr_stack.pop();
                if expr_opt.is_none() {
                    compiler_err!(self.location, "invalid expression");
                }
                let expr = expr_opt.unwrap().unwrap();
                let location = *expr.get_location();
                self.expr_stack.push(Some(Box::new(
                    (&SingularExpressionNode {
                        location,
                        operation: sin_op,
                        expr,
                    })
                        .into(),
                )));
            }
            AnyOperation::Binary(bin_op) => {
                let right_opt = self.expr_stack.pop();
                let left_opt = self.expr_stack.pop();
                if right_opt.is_none() || left_opt.is_none() {
                    compiler_err!(self.location, "invalid expression");
                }

                let right = right_opt.unwrap().unwrap();
                let left = left_opt.unwrap().unwrap();

                let location = *left.get_location();
                self.expr_stack.push(Some(Box::new(
                    (&BinaryExpressionNode {
                        location,
                        operation: bin_op,
                        left,
                        right,
                    })
                        .into(),
                )));
            }
            AnyOperation::None => {
                compiler_err!(self.location, "invalid expression")
            }
        };

        Ok(())
    }

    fn build(&mut self) -> CompilerResult<ExpressionBox> {
        while !self.op_stack.is_empty() {
            self.process_operator()?;
        }

        if self.expr_stack.len() != 1 {
            compiler_err!(self.location, "invalid expression");
        }

        let expr = take(&mut self.expr_stack[0]);
        match expr {
            Some(expr_value) => Ok(expr_value),
            None => compiler_err!(self.location, "invalid expression"),
        }
    }
}

pub struct Parser<'tkn> {
    reader: &'tkn mut TokenReader<'tkn>,
}

impl<'tkn, 'ctx, 'st> Parser<'tkn>
where
    'st: 'ctx,
{
    pub fn new(reader: &'tkn mut TokenReader<'tkn>) -> Parser<'tkn> {
        Self { reader }
    }

    pub fn parse(&mut self) -> CompilerResult<SourceUnit> {
        let mut result = SourceUnit { body: Vec::new() };

        while self.reader.has_tokens() {
            let token = self.reader.next()?;
            let location = token.location;
            if let TokenType::Keyword(kw) = &token.token_type {
                match kw {
                    Keyword::Fn => {
                        let func: GlobalStatementBox = Box::new(
                            (&self.parse_function(location, FunctionLinkage::Standard)?).into(),
                        );
                        result.body.push(func);
                    }
                    Keyword::Extern => {
                        self.reader.expect_token(TokenType::Keyword(Keyword::Fn))?;

                        let extern_func: GlobalStatementBox = Box::new(
                            (&self.parse_function(location, FunctionLinkage::External)?).into(),
                        );
                        result.body.push(extern_func);
                    }
                    Keyword::Const => {
                        let const_expr: GlobalStatementBox =
                            Box::new((&self.parse_const_decl(location)?).into());
                        result.body.push(const_expr);
                    }
                    Keyword::Struct => {
                        let struct_node: GlobalStatementBox =
                            Box::new((&self.parse_struct(location)?).into());
                        result.body.push(struct_node);
                    }
                    Keyword::Import => {
                        let import_node: GlobalStatementBox =
                            Box::new((&self.parse_import(location)?).into());
                        result.body.push(import_node);
                    }
                    _ => compiler_err!(location, "unexpected token: {:?}", kw),
                }
            }
        }

        Ok(result)
    }

    fn parse_function(
        &mut self,
        location: Location,
        linkage: FunctionLinkage,
    ) -> CompilerResult<FunctionNode> {
        let paren_or_dot = self.reader.find_one_of(&[
            TokenType::Operator(OperatorType::LeftParen),
            TokenType::Operator(OperatorType::Dot),
        ])?;
        let is_method = paren_or_dot.token_type == TokenType::Operator(OperatorType::Dot);
        let mut self_type: Option<Type> = None;

        if is_method {
            self_type = Some(self.parse_type()?);
            self.reader
                .expect_token(TokenType::Operator(OperatorType::Dot))?;
        }

        let name = self.reader.expect_identifier()?;
        self.reader
            .expect_token(TokenType::Operator(OperatorType::LeftParen))?;

        let mut params: Vec<FunctionArg> = Vec::new();

        let peeked_paren = self.reader.peek()?;
        if peeked_paren.token_type != TokenType::Operator(OperatorType::RightParen) {
            loop {
                let is_ref = self
                    .reader
                    .skip_token_if_present(TokenType::Keyword(Keyword::Ref))?;

                let self_loc = self
                    .reader
                    .skip_token_if_present_with_loc(TokenType::Keyword(Keyword::SelfKw))?;

                if let Some(loc) = self_loc {
                    params.push(FunctionArg {
                        location: loc,
                        name: "self".into(),
                        arg_type: wrap_option(loc, self_type.as_ref(), "invalid expression")?
                            .clone(),
                        is_ref,
                    });
                } else {
                    let (arg_loc, arg_name) = self.reader.expect_identifier_with_loc()?;
                    self.reader
                        .expect_token(TokenType::Operator(OperatorType::Colon))?;
                    let arg_type = self.parse_type()?;
                    params.push(FunctionArg {
                        location: arg_loc,
                        name: arg_name,
                        arg_type,
                        is_ref,
                    });
                }

                let following = self.reader.next()?;
                if following.token_type == TokenType::Operator(OperatorType::RightParen) {
                    break;
                }
            }
        } else {
            self.reader.next()?;
        }

        let token = self.reader.next()?.clone();
        let (ret_type, has_scope) = match token.token_type {
            TokenType::Operator(OperatorType::Colon) => {
                let ret_type = self.parse_type()?;
                let next_token = self.reader.next()?;
                match next_token.token_type {
                    TokenType::Operator(OperatorType::LeftBrace) => (ret_type, true),
                    TokenType::Operator(OperatorType::Semicolon) => (ret_type, false),
                    _ => {
                        compiler_err!(next_token.location, "invalid token {:?}", token.token_type);
                    }
                }
            }
            TokenType::Operator(OperatorType::LeftBrace) => (Type::Void, true),
            TokenType::Operator(OperatorType::Semicolon) => (Type::Void, false),
            _ => {
                compiler_err!(token.location, "invalid token {:?}", token.token_type);
            }
        };

        let scope = if has_scope {
            let mut scope_node = self.parse_scope("entry")?;

            let is_last_stmt_ret = match scope_node.body.last() {
                Some(stmt) => match stmt.as_ref() {
                    AnyStatementNode::ReturnNode(_) => true,
                    _ => false,
                },
                None => false,
            };

            if !is_last_stmt_ret {
                if ret_type != Type::Void {
                    compiler_err!(location, "missing return at the end of function");
                }

                scope_node.body.push(Box::new(
                    (&ReturnNode {
                        location,
                        expression: None,
                    })
                        .into(),
                ));
            }

            Some(scope_node)
        } else {
            None
        };

        Ok(FunctionNode {
            location,
            self_type,
            name,
            params,
            ret_type,
            linkage,
            scope,
        })
    }

    fn parse_type(&mut self) -> CompilerResult<Type> {
        let token = self.reader.next()?;
        match &token.token_type {
            TokenType::Keyword(kw) => {
                let arg_type_res = Type::from_keyword(kw);
                match arg_type_res {
                    Some(arg_type) => Ok(arg_type),
                    None => compiler_err!(token.location, "invalid type {:?}", kw),
                }
            }
            TokenType::Identifier(iden) => Ok(Type::Alias(iden.clone())),
            _ => {
                compiler_err!(token.location, "invalid token {:?}", token.token_type)
            }
        }
    }

    fn parse_scope(&mut self, name: &str) -> CompilerResult<ScopeNode> {
        let mut scope = ScopeNode {
            body: Vec::new(),
            name: name.to_string(),
        };

        let peeked_brace = self.reader.peek()?;
        if peeked_brace.token_type == TokenType::Operator(OperatorType::RightBrace) {
            self.reader.next()?;
            return Ok(scope);
        }

        loop {
            scope.body.push(self.parse_statement()?);
            let peeked_brace = self.reader.peek()?;
            if peeked_brace.token_type == TokenType::Operator(OperatorType::RightBrace) {
                self.reader.next()?;
                break;
            }
        }

        Ok(scope)
    }

    fn parse_statement(&mut self) -> CompilerResult<StatementBox> {
        let token = self.reader.next()?;
        let location = token.location;
        if let TokenType::Keyword(kw) = &token.token_type {
            match kw {
                Keyword::Return => {
                    let peek = self.reader.peek()?;
                    return if peek.token_type == TokenType::Operator(OperatorType::Semicolon) {
                        self.reader.next()?;
                        Ok(Box::new(
                            (&ReturnNode {
                                location,
                                expression: None,
                            })
                                .into(),
                        ))
                    } else {
                        let expression = self.parse_expression(OperatorType::Semicolon)?;
                        Ok(Box::new(
                            (&ReturnNode {
                                location,
                                expression: Some(expression),
                            })
                                .into(),
                        ))
                    };
                }
                Keyword::Var => {
                    let name = self.reader.expect_identifier()?;

                    let peek_colon = self.reader.peek()?;

                    let mut var_type: Option<Type> = None;
                    if peek_colon.token_type == TokenType::Operator(OperatorType::Colon) {
                        self.reader.next()?;
                        var_type = Some(self.parse_type()?);
                    }

                    let mut expression: Option<ExpressionBox> = None;
                    let peek_value = self.reader.peek()?;
                    if peek_value.token_type == TokenType::Operator(OperatorType::Equals) {
                        self.reader.next()?;
                        expression = Some(self.parse_expression(OperatorType::Semicolon)?);
                    } else {
                        self.reader
                            .expect_token(TokenType::Operator(OperatorType::Semicolon))?;
                    }

                    return Ok(Box::new(
                        (&VarDeclNode {
                            location,
                            name,
                            expression,
                            var_type,
                        })
                            .into(),
                    ));
                }
                Keyword::Ref => {
                    let name = self.reader.expect_identifier()?;

                    let peek_colon = self.reader.peek()?;

                    let mut var_type: Option<Type> = None;
                    if peek_colon.token_type == TokenType::Operator(OperatorType::Colon) {
                        self.reader.next()?;
                        var_type = Some(self.parse_type()?);
                    }

                    self.reader
                        .expect_token(TokenType::Operator(OperatorType::Equals))?;

                    let expression = self.parse_expression(OperatorType::Semicolon)?;

                    return Ok(Box::new(
                        (&RefDeclNode {
                            location,
                            name,
                            expression,
                            var_type,
                        })
                            .into(),
                    ));
                }
                Keyword::If => {
                    return Ok(Box::new((&self.parse_if_statement(location)?).into()));
                }
                Keyword::While => {
                    return Ok(Box::new((&self.parse_while_statement(location)?).into()));
                }
                _ => {}
            }
        }

        self.reader.seek_back();
        let expr = self.parse_expression(OperatorType::Semicolon)?;

        Ok(Box::new(
            (&ExpressionStatementNode { expression: expr }).into(),
        ))
    }

    fn parse_if_statement(&mut self, location: Location) -> CompilerResult<IfNode> {
        let expr = self.parse_expression(OperatorType::LeftBrace)?;
        let scope_name = get_random_identifier("if_body");
        let scope = self.parse_scope(&scope_name)?;

        let else_peek = self.reader.peek()?;
        let else_scope = if else_peek.token_type == TokenType::Keyword(Keyword::Else) {
            self.reader.next()?;
            let peek_lb = self.reader.peek()?;
            let scope_name = get_random_identifier("if_else");
            if peek_lb.token_type == TokenType::Operator(OperatorType::LeftBrace) {
                self.reader.next()?;
                Some(self.parse_scope(&scope_name)?)
            } else {
                let mut stmts = Vec::new();
                stmts.push(self.parse_statement()?);
                Some(ScopeNode {
                    body: stmts,
                    name: scope_name,
                })
            }
        } else {
            None
        };

        Ok(IfNode {
            location,
            condition: expr,
            then_scope: scope,
            else_scope,
        })
    }

    fn parse_while_statement(&mut self, location: Location) -> CompilerResult<WhileNode> {
        let expr = self.parse_expression(OperatorType::LeftBrace)?;
        let scope_name = get_random_identifier("while_body");
        let scope = self.parse_scope(&scope_name)?;
        Ok(WhileNode {
            location,
            condition: expr,
            scope,
        })
    }

    fn parse_expression(&mut self, stop_op: OperatorType) -> CompilerResult<ExpressionBox> {
        self.parse_expression_until_one_of(&[stop_op])
    }

    fn parse_member_expression(
        &mut self,
        location: Location,
        builder: &mut ExpressionBuilder,
        object_expr: ExpressionBox,
    ) -> CompilerResult<()> {
        self.reader.next()?;
        let (field_loc, field_name) = self.reader.expect_identifier_with_loc()?;
        if self
            .reader
            .skip_token_if_present(TokenType::Operator(OperatorType::LeftParen))?
        {
            let func_call = self.parse_function_call(
                field_loc,
                Identifier {
                    namespace: None,
                    value: field_name.to_string(),
                },
            )?;
            builder.push_expr(
                (&MethodCall {
                    location,
                    object_expr,
                    name: field_name,
                    args: func_call.args,
                })
                    .into(),
            );
        } else {
            builder.push_expr(
                (&GetFieldNode {
                    location,
                    object_expr,
                    field_name,
                })
                    .into(),
            );
        }

        Ok(())
    }

    fn parse_identifier_continuation(
        &mut self,
        builder: &mut ExpressionBuilder,
        location: &Location,
        identifier: Identifier,
    ) -> CompilerResult<()> {
        let peeked = self.reader.peek()?.clone();
        match peeked.token_type {
            TokenType::Operator(OperatorType::LeftParen) => {
                self.reader.next()?;
                let call = self.parse_function_call(peeked.location, identifier)?;
                builder.push_expr((&call).into());
            }
            TokenType::Operator(OperatorType::LeftSquare) => {
                self.reader.next()?;
                let expr = self.parse_expression(OperatorType::RightSquare)?;
                builder.push_expr(
                    (&GetElementNode {
                        location: *location,
                        object: Box::new(
                            (&IdentifierNode {
                                location: *location,
                                name: identifier,
                            })
                                .into(),
                        ),
                        index: expr,
                    })
                        .into(),
                );
            }
            TokenType::Operator(OperatorType::Dot) => {
                self.parse_member_expression(
                    location.clone(),
                    builder,
                    Box::new(
                        (&IdentifierNode {
                            location: *location,
                            name: identifier,
                        })
                            .into(),
                    ),
                )?;
            }
            TokenType::Operator(OperatorType::DoubleColon) => {
                self.reader.next()?;
                let identifier_str = self.reader.expect_identifier()?;
                self.parse_identifier_continuation(
                    builder,
                    location,
                    Identifier {
                        namespace: Some(identifier.value),
                        value: identifier_str,
                    },
                )?;
            }
            _ => {
                builder.push_expr(
                    (&IdentifierNode {
                        location: *location,
                        name: identifier,
                    })
                        .into(),
                );
            }
        }

        Ok(())
    }

    fn parse_expression_until_one_of(
        &mut self,
        stop_ops: &[OperatorType],
    ) -> CompilerResult<ExpressionBox> {
        let peek = self.reader.peek();
        let token_start_loc = match peek {
            Ok(tkn) => tkn.location,
            Err(err) => err.location,
        };

        let mut builder = ExpressionBuilder::new(token_start_loc.clone());

        loop {
            let token = self.reader.next()?.clone();

            match &token.token_type {
                TokenType::Operator(op_type) => {
                    if stop_ops.iter().any(|op| *op_type == *op) {
                        break;
                    } else if *op_type == OperatorType::LeftParen {
                        builder.push_expr_box(self.parse_expression(OperatorType::RightParen)?);
                    } else {
                        builder.push_op(op_type)?;
                    }
                }
                TokenType::Number(num) => {
                    builder.push_expr(
                        (&NumberNode {
                            location: token.location,
                            number: *num,
                        })
                            .into(),
                    );
                }
                TokenType::Identifier(value) => {
                    self.parse_identifier_continuation(
                        &mut builder,
                        &token.location,
                        Identifier {
                            namespace: None,
                            value: value.clone(),
                        },
                    )?;
                }
                TokenType::Keyword(kw) => match kw {
                    Keyword::SelfKw => {
                        let peeked = self.reader.peek()?.clone();
                        if peeked.token_type == TokenType::Operator(OperatorType::Dot) {
                            self.parse_member_expression(
                                token.location.clone(),
                                &mut builder,
                                Box::new(
                                    (&SelfNode {
                                        location: token.location,
                                    })
                                        .into(),
                                ),
                            )?;
                        } else {
                            builder.push_expr(
                                (&SelfNode {
                                    location: token.location,
                                })
                                    .into(),
                            );
                        }
                    }
                    Keyword::Null => {
                        builder.push_expr(
                            (&NullNode {
                                location: token.location,
                            })
                                .into(),
                        );
                    }
                    _ => {
                        let cast_type = wrap_option(
                            token.location,
                            Type::from_keyword(kw),
                            "unexpected keyword",
                        )?;
                        self.reader
                            .expect_token(TokenType::Operator(OperatorType::LeftParen))?;
                        let expr = self.parse_expression(OperatorType::RightParen)?;
                        builder.push_expr(
                            (&CastNode {
                                location: token.location,
                                target_type: cast_type,
                                expr,
                            })
                                .into(),
                        );
                    }
                },
                TokenType::String(str) => {
                    builder.push_expr(
                        (&StringNode {
                            location: token.location,
                            value: str.to_string(),
                        })
                            .into(),
                    );
                }
            }
        }

        builder.build()
    }

    fn parse_function_call(
        &mut self,
        location: Location,
        name: Identifier,
    ) -> CompilerResult<FunctionCall> {
        let mut args = Vec::new();

        let peek = self.reader.peek()?;
        if peek.token_type == TokenType::Operator(OperatorType::RightParen) {
            self.reader.next()?;
            return Ok(FunctionCall {
                location,
                name,
                args,
            });
        }

        loop {
            let expr = self
                .parse_expression_until_one_of(&[OperatorType::Comma, OperatorType::RightParen])?;
            args.push(expr);

            self.reader.seek_back();

            let expr_term = self.reader.next()?;
            if expr_term.token_type == TokenType::Operator(OperatorType::RightParen) {
                break;
            }
        }

        Ok(FunctionCall {
            location,
            name,
            args,
        })
    }

    fn parse_const_decl(&mut self, location: Location) -> CompilerResult<ConstDeclNode> {
        let name = self.reader.expect_identifier()?;
        let eq_or_colon = self.reader.next()?.clone();

        let const_type = match eq_or_colon.token_type {
            TokenType::Operator(OperatorType::Colon) => {
                let parsed_type = self.parse_type()?;
                self.reader
                    .expect_token(TokenType::Operator(OperatorType::Equals))?;
                Some(parsed_type)
            }
            TokenType::Operator(OperatorType::Equals) => None,
            token => compiler_err!(eq_or_colon.location, "unexpected token: {:?}", token),
        };

        let value = self.parse_expression(OperatorType::Semicolon)?;

        Ok(ConstDeclNode {
            location,
            name,
            const_type,
            value,
        })
    }

    fn parse_struct(&mut self, location: Location) -> CompilerResult<StructNode> {
        let name = self.reader.expect_identifier()?;
        self.reader
            .expect_token(TokenType::Operator(OperatorType::LeftBrace))?;

        let mut fields = Vec::new();
        loop {
            let peek = self.reader.peek()?.clone();
            if peek.token_type == TokenType::Operator(OperatorType::RightBrace) {
                break;
            }

            let field_name = self.reader.expect_identifier()?;
            self.reader
                .expect_token(TokenType::Operator(OperatorType::Colon))?;

            let field_type = self.parse_type()?;

            fields.push(StructField {
                location: peek.location,
                name: field_name,
                field_type,
            });

            self.reader
                .skip_token_if_present(TokenType::Operator(OperatorType::Comma))?;
        }

        Ok(StructNode {
            location,
            name,
            fields,
        })
    }

    fn parse_import(&mut self, location: Location) -> CompilerResult<ImportNode> {
        let module_name = self.reader.expect_identifier()?;
        self.reader
            .expect_token(TokenType::Operator(OperatorType::Semicolon))?;
        Ok(ImportNode {
            location,
            module_name,
        })
    }
}
