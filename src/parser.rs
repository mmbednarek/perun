use rand::{distributions::Alphanumeric, Rng};

use crate::token_reader::TokenReader;
use crate::token::{TokenType, OperatorType, Location, Keyword};
use crate::error::{wrap_option, CompilerResult};
use crate::ast::*;
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

struct ExpressionBuilder<'ctx, 'st>
    where 'st: 'ctx {
    expr_stack: Vec<Option<Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>>>,
    op_stack: Vec<AnyOperation>,
    last_is_op: bool,
    location: Location,
}

impl<'ctx, 'st> ExpressionBuilder<'ctx, 'st> {
    fn new(location: Location) -> Self {
        Self{expr_stack: Vec::new(), op_stack: Vec::new(), last_is_op: true, location}
    }

    fn push_expr<T: ExpressionNode<'ctx, 'st> + 'ctx>(&mut self, node: T) {
        self.expr_stack.push(Some(Box::new(node)));
        self.last_is_op = false;
    }

    fn push_expr_box(&mut self, node: Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>) {
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
        while last_op.proceeds(&op) {
            self.process_operator()?;
            last_op = self.last_op();
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
                self.expr_stack.push(Some(Box::new(SingularExpressionNode{location, operation: sin_op, expr})));
            },
            AnyOperation::Binary(bin_op) => {
                let right_opt = self.expr_stack.pop();
                let left_opt = self.expr_stack.pop();
                if right_opt.is_none() || left_opt.is_none() {
                    compiler_err!(self.location, "invalid expression");
                }

                let right = right_opt.unwrap().unwrap();
                let left = left_opt.unwrap().unwrap();

                let location = *left.get_location();
                self.expr_stack.push(Some(Box::new(BinaryExpressionNode{location, operation: bin_op, left, right})));
            }
            AnyOperation::None => {compiler_err!(self.location, "invalid expression")},
        };

        Ok(())
    }

    fn build(&mut self) -> CompilerResult<Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>> {
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
where 'st: 'ctx {
    pub fn new(reader: &'tkn mut TokenReader<'tkn>) -> Parser {
        Self{reader}
    }

    pub fn parse(&mut self) -> CompilerResult<SourceUnit<'ctx, 'st>> {
        let mut result = SourceUnit{body: Vec::new()};

        while self.reader.has_tokens() {
            let token = self.reader.next()?;
            let location = token.location;
            if let TokenType::Keyword(kw) = &token.token_type {
                match kw {
                    Keyword::Fn => {
                        let func: Box<dyn GlobalStatementNode<'ctx, 'st> + 'ctx> = Box::new(self.parse_function(location, FunctionLinkage::Standard)?);
                        result.body.push(func);
                    },
                    Keyword::Extern => {
                        self.reader.expect_token(TokenType::Keyword(Keyword::Fn))?;

                        let extern_func: Box<dyn GlobalStatementNode<'ctx, 'st> + 'ctx> = Box::new(self.parse_function(location, FunctionLinkage::External)?);
                        result.body.push(extern_func);
                    },
                    Keyword::Const => {
                        let const_expr: Box<dyn GlobalStatementNode<'ctx, 'st> + 'ctx> = Box::new(self.parse_const_decl(location)?);
                        result.body.push(const_expr);
                    },
                    Keyword::Struct => {
                        let struct_node: Box<dyn GlobalStatementNode<'ctx, 'st> + 'ctx> = Box::new(self.parse_struct(location)?);
                        result.body.push(struct_node);
                    },
                    _ => compiler_err!(location, "unexpected token: {:?}", kw),
                }
            }

        }

        Ok(result)
    }

    fn parse_function(&mut self, location: Location, linkage: FunctionLinkage) -> CompilerResult<FunctionNode<'ctx, 'st>> {
        let name = self.reader.expect_identifier()?;
        self.reader.expect_token(TokenType::Operator(OperatorType::LeftParen))?;

        let mut params: Vec<FunctionArg> = Vec::new();

        let peeked_paren = self.reader.peek()?;
        if peeked_paren.token_type != TokenType::Operator(OperatorType::RightParen) {
            loop {
                let arg_name = self.reader.expect_identifier()?;
                self.reader.expect_token(TokenType::Operator(OperatorType::Colon))?;
                let arg_type = self.parse_type()?;
                params.push(FunctionArg{name: arg_name, arg_type});
                

                let following = self.reader.next()?;
                if following.token_type == TokenType::Operator(OperatorType::RightParen) {
                    break
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
            },
            TokenType::Operator(OperatorType::LeftBrace) => (Type::Void, true),
            TokenType::Operator(OperatorType::Semicolon) => (Type::Void, false),
            _ => {
                compiler_err!(token.location, "invalid token {:?}", token.token_type);
            }
        };

        let scope = if has_scope {
            let mut scope_node = self.parse_scope("entry")?;

            let is_last_stmt_ret = match scope_node.body.last() {
                Some(stmt) => {
                    match stmt.to_any_statement_node() {
                        AnyStatementNode::ReturnNode(_) => true,
                        _ => false,
                    }
                },
                None => false,
            };

            if !is_last_stmt_ret {
                if ret_type != Type::Void {
                    compiler_err!(location, "missing return at the end of function");
                }

                scope_node.body.push(Box::new(ReturnNode{location, expression: None}));
            }

            Some(scope_node)
        } else { None };

        Ok(FunctionNode{location, name, params, ret_type, linkage, scope})
    }

    fn parse_type(&mut self) -> CompilerResult<Type> {
        let token = self.reader.next()?;
        match &token.token_type {
            TokenType::Keyword(kw) => {
                let arg_type_res = Type::from_keyword(kw);
                match arg_type_res  {
                    Some(arg_type) => Ok(arg_type),
                    None => compiler_err!(token.location, "invalid type {:?}", kw),
                }
            },
            TokenType::Identifier(iden) => {
                Ok(Type::Alias(iden.clone()))
            },
            _ => { compiler_err!(token.location, "invalid token {:?}", token.token_type) }
        }
    }

    fn parse_scope(&mut self, name: &str) -> CompilerResult<ScopeNode<'ctx, 'st>> {
        let mut scope = ScopeNode::<'ctx, 'st>{body: Vec::new(), name: name.to_string()};

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

    fn parse_statement(&mut self) -> CompilerResult<Box<dyn StatementNode<'ctx, 'st> + 'ctx>> {
        let token = self.reader.next()?;
        let location = token.location;
        if let TokenType::Keyword(kw) = &token.token_type {
            match kw {
                Keyword::Return => {
                    let peek = self.reader.peek()?;
                    if peek.token_type == TokenType::Operator(OperatorType::Semicolon) {
                        self.reader.next()?;
                        return Ok(Box::new(ReturnNode{location, expression: None}));
                    } else {
                        let expression = self.parse_expression(OperatorType::Semicolon)?;
                        return Ok(Box::new(ReturnNode{location, expression: Some(expression)}));
                    }
                },
                Keyword::Var => {
                    let name = self.reader.expect_identifier()?;

                    let peek_colon = self.reader.peek()?;

                    let mut var_type: Option<Type> = None;
                    if peek_colon.token_type == TokenType::Operator(OperatorType::Colon) {
                        self.reader.next()?;
                        var_type = Some(self.parse_type()?);
                    }
                    
                    let mut expression: Option<ExpressionBox<'ctx, 'st>> = None;
                    let peek_value = self.reader.peek()?;
                    if peek_value.token_type == TokenType::Operator(OperatorType::Equals) {
                        self.reader.next()?;
                        expression = Some(self.parse_expression(OperatorType::Semicolon)?);
                    } else {
                        self.reader.expect_token(TokenType::Operator(OperatorType::Semicolon))?;
                    }

                    return Ok(Box::new(VarDeclNode{location, name, expression, var_type}));
                },
                Keyword::Ref => {
                    let name = self.reader.expect_identifier()?;

                    let peek_colon = self.reader.peek()?;

                    let mut var_type: Option<Type> = None;
                    if peek_colon.token_type == TokenType::Operator(OperatorType::Colon) {
                        self.reader.next()?;
                        var_type = Some(self.parse_type()?);
                    }

                    self.reader.expect_token(TokenType::Operator(OperatorType::Equals))?;
                    
                    let expression = self.parse_expression(OperatorType::Semicolon)?;

                    return Ok(Box::new(RefDeclNode{location, name, expression, var_type}));
                },
                Keyword::If => {
                    return Ok(Box::new(self.parse_if_statement(location)?));
                },
                Keyword::While => {
                    return Ok(Box::new(self.parse_while_statement(location)?));
                },
                _ => {},
            }
        }

        self.reader.seek_back();
        let expr = self.parse_expression(OperatorType::Semicolon)?;

        Ok(Box::new(ExpressionStatementNode{expression: expr}))
    }

    fn parse_if_statement(&mut self, location: Location) -> CompilerResult<IfNode<'ctx, 'st>> {
        let expr = self.parse_expression(OperatorType::LeftBrace)?;
        let scope_name = get_random_identifier("if_body");
        let scope = self.parse_scope(&scope_name)?;
        Ok(IfNode{location, condition: expr, iftrue_scope: scope})
    }

    fn parse_while_statement(&mut self, location: Location) -> CompilerResult<WhileNode<'ctx, 'st>> {
        let expr = self.parse_expression(OperatorType::LeftBrace)?;
        let scope_name = get_random_identifier("while_body");
        let scope = self.parse_scope(&scope_name)?;
        Ok(WhileNode{location, condition: expr, scope})
    }

    fn parse_expression(&mut self, stop_op: OperatorType) -> CompilerResult<Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>> {
        self.parse_expression_until_one_of(&[stop_op])
    }

    fn parse_expression_until_one_of(&mut self, stop_ops: &[OperatorType]) -> CompilerResult<Box<dyn ExpressionNode<'ctx, 'st> + 'ctx>> {
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
                },
                TokenType::Number(num) => {
                    builder.push_expr(NumberNode{location: token.location, number: *num});
                },
                TokenType::Identifier(value) => {
                    let peeked = self.reader.peek()?.clone();
                    if peeked.token_type == TokenType::Operator(OperatorType::LeftParen) {
                        self.reader.next()?;
                        let call = self.parse_function_call(peeked.location, value.as_ref())?;
                        builder.push_expr(call);
                    } else if peeked.token_type == TokenType::Operator(OperatorType::LeftSquare) {
                        self.reader.next()?;
                        let expr = self.parse_expression(OperatorType::RightSquare)?;
                        builder.push_expr(GetElementNode{location: token.location, object: Box::new(IdentifierNode{location: token.location, name: value.to_string()}), index: expr});
                    } else if peeked.token_type == TokenType::Operator(OperatorType::Dot) {
                        self.reader.next()?;
                        let field_name = self.reader.expect_identifier()?;
                        builder.push_expr(GetFieldNode{location: token.location, object_expr: Box::new(IdentifierNode{location: token.location, name: value.to_string()}), field_name});
                    } else {
                        builder.push_expr(IdentifierNode{location: token.location, name: value.to_string()});
                    }
                },
                TokenType::Keyword(kw) => {
                    let cast_type = wrap_option(token.location, Type::from_keyword(kw), "unexpected keyword")?;
                    self.reader.expect_token(TokenType::Operator(OperatorType::LeftParen))?;
                    let expr = self.parse_expression(OperatorType::RightParen)?;
                    builder.push_expr(CastNode{location: token.location, target_type: cast_type, expr});
                },
                TokenType::String(str) => {
                    builder.push_expr(StringNode{location: token.location, value: str.to_string()});
                },
            }
        }

        return builder.build();
    }

    fn parse_function_call(&mut self, location: Location, name: &str) -> CompilerResult<FunctionCall<'ctx, 'st>> {
        let mut args = Vec::new();

        let peek = self.reader.peek()?;
        if peek.token_type == TokenType::Operator(OperatorType::RightParen) {
            self.reader.next()?;
            return Ok(FunctionCall{location, name: name.to_string(), args});
        }

        loop {
            let expr = self.parse_expression_until_one_of(&[OperatorType::Comma, OperatorType::RightParen])?;
            args.push(expr);

            self.reader.seek_back();

            let expr_term = self.reader.next()?;
            if expr_term.token_type == TokenType::Operator(OperatorType::RightParen) {
                break;
            }
        }

        Ok(FunctionCall{location, name: name.to_string(), args})
    }

    fn parse_const_decl(&mut self, location: Location) -> CompilerResult<ConstDeclNode<'ctx, 'st>> {
        let name = self.reader.expect_identifier()?;
        let eq_or_colon = self.reader.next()?.clone();

        let const_type = match eq_or_colon.token_type {
            TokenType::Operator(OperatorType::Colon) => {
                let parsed_type = self.parse_type()?;
                self.reader.expect_token(TokenType::Operator(OperatorType::Equals))?;
                Some(parsed_type)
            },
            TokenType::Operator(OperatorType::Equals) => {
                None
            },
            token => compiler_err!(eq_or_colon.location, "unexpected token: {:?}", token),
        };

        let value = self.parse_expression(OperatorType::Semicolon)?;

        Ok(ConstDeclNode{location, name, const_type, value})
    }

    fn parse_struct(&mut self, location: Location) -> CompilerResult<StructNode> {
        let name = self.reader.expect_identifier()?;
        self.reader.expect_token(TokenType::Operator(OperatorType::LeftBrace))?;

        let mut fields = Vec::new();
        loop {
            let peek = self.reader.peek()?.clone();
            if peek.token_type == TokenType::Operator(OperatorType::RightBrace) {
                break;
            }

            let field_name = self.reader.expect_identifier()?;
            self.reader.expect_token(TokenType::Operator(OperatorType::Colon))?;

            let field_type = self.parse_type()?;

            fields.push(StructField{location: peek.location, name: field_name, field_type});

            self.reader.skip_token_if_present(TokenType::Operator(OperatorType::Comma))?;
        }

        Ok(StructNode{location, name, fields})
    }
}
