use crate::token_reader::TokenReader;
use crate::token::{TokenType, OperatorType, Location, Keyword};
use crate::error::CompilerResult;
use crate::ast::*;
use crate::typing::Type;
use std::mem::take;

struct ExpressionBuilder<'a> {
    expr_stack: Vec<Option<Box<dyn ExpressionNode<'a, 'a> + 'a>>>,
    op_stack: Vec<BinaryOperation>,
    location: Location,
}

impl<'a> ExpressionBuilder<'a> {
    fn new(location: Location) -> Self {
        Self{expr_stack: Vec::new(), op_stack: Vec::new(), location}
    }

    fn push_expr<T: ExpressionNode<'a, 'a> + 'a>(&mut self, node: T) {
        self.expr_stack.push(Some(Box::new(node)));
    }

    fn push_expr_box(&mut self, node: Box<dyn ExpressionNode<'a, 'a> + 'a>) {
        self.expr_stack.push(Some(node));
    }

    fn push_op(&mut self, op: BinaryOperation) -> CompilerResult<()> {
        let mut last_op = self.last_op();
        while last_op.proceeds(op) {
            self.process_operator()?;
            last_op = self.last_op();
        }
        self.op_stack.push(op);
        Ok(())
    }

    fn last_op(&self) -> BinaryOperation {
        *self.op_stack.last().unwrap_or(&BinaryOperation::None)
    }

    fn process_operator(&mut self) -> CompilerResult<()> {
        let op_res = self.op_stack.pop();
        if op_res.is_none() {
            compiler_err!(self.location, "no operators left");
        }

        let op = op_res.unwrap();

        let right_opt = self.expr_stack.pop();
        let left_opt = self.expr_stack.pop();
        if right_opt.is_none() || left_opt.is_none() {
            compiler_err!(self.location, "invalid expression");
        }

        let right = right_opt.unwrap().unwrap();
        let left = left_opt.unwrap().unwrap();

        let location = *left.get_location();
        self.expr_stack.push(Some(Box::new(BinaryExpressionNode{location, operation: op, left, right})));
        Ok(())
    }

    fn build(&mut self) -> CompilerResult<Box<dyn ExpressionNode<'a, 'a> + 'a>> {
        while !self.op_stack.is_empty() {
            self.process_operator();
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

pub struct Parser<'a> {
    reader: &'a mut TokenReader<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(reader: &'a mut TokenReader<'a>) -> Parser {
        Self{reader}
    }

    pub fn parse(&mut self) -> CompilerResult<SourceUnit<'a, 'a>> {
        let mut result = SourceUnit::<'a, 'a>{body: Vec::new()};

        while self.reader.has_tokens() {
            let token = self.reader.next()?;
            let location = token.location;
            if let TokenType::Keyword(kw) = &token.token_type {
                if *kw == Keyword::Fn {
                    let func: Box<dyn GlobalStatementNode<'a, 'a>> = Box::new(self.parse_function(location)?);
                    result.body.push(func);
                }
            }

        }

        Ok(result)
    }

    fn parse_function(&mut self, location: Location) -> CompilerResult<FunctionNode<'a, 'a>> {
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

        let mut ret_type = Type::Void;

        let token = self.reader.next()?;
        match token.token_type {
            TokenType::Operator(OperatorType::Colon) => {
                ret_type = self.parse_type()?;
                self.reader.expect_token(TokenType::Operator(OperatorType::LeftBrace))?;
            },
            TokenType::Operator(OperatorType::LeftBrace) => {},
            _ => {
                compiler_err!(token.location, "invalid token {:?}", token.token_type);
            }
        }

        let scope = self.parse_scope("entry")?;

        Ok(FunctionNode::<'a, 'a>{location, name, params, scope, ret_type})
    }

    fn parse_type(&mut self) -> CompilerResult<Type> {
        let (kw_loc, arg_type_kw) = self.reader.expect_keyword_with_loc()?;
        let arg_type_res = Type::from_keyword(&arg_type_kw);
        match arg_type_res  {
            Some(arg_type) => Ok(arg_type),
            None => compiler_err!(kw_loc, "invalid type {:?}", arg_type_kw),
        }
    }

    fn parse_scope(&mut self, name: &str) -> CompilerResult<ScopeNode<'a, 'a>> {
        let mut scope = ScopeNode::<'a, 'a>{body: Vec::new(), name: name.to_string()};

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

    fn parse_statement(&mut self) -> CompilerResult<Box<dyn StatementNode<'a, 'a> + 'a>> {
        let token = self.reader.next()?;
        let location = token.location;
        if let TokenType::Keyword(kw) = &token.token_type {
            match kw {
                Keyword::Return => {
                    let expression = self.parse_expression(OperatorType::Semicolon)?;
                    return Ok(Box::new(ReturnNode::<'a, 'a>{location, expression}));
                },
                Keyword::Var => {
                    let name = self.reader.expect_identifier()?;
                    let token = self.reader.next()?;

                    let mut var_type: Option<Type> = None;
                    if token.token_type == TokenType::Operator(OperatorType::Colon) {
                        var_type = Some(self.parse_type()?);
                        self.reader.expect_token(TokenType::Operator(OperatorType::Equals))?;
                    } else if token.token_type != TokenType::Operator(OperatorType::Equals) {
                        compiler_err!(location, "expected equal sign");
                    }

                    let expression = self.parse_expression(OperatorType::Semicolon)?;
                    return Ok(Box::new(VarDeclNode::<'a, 'a>{location, name, expression, var_type}));
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

    fn parse_if_statement(&mut self, location: Location) -> CompilerResult<IfNode<'a, 'a>> {
        let expr = self.parse_expression(OperatorType::LeftBrace)?;
        let scope = self.parse_scope("iftrue")?;
        Ok(IfNode{location, condition: expr, iftrue_scope: scope})
    }

    fn parse_while_statement(&mut self, location: Location) -> CompilerResult<WhileNode<'a, 'a>> {
        let expr = self.parse_expression(OperatorType::LeftBrace)?;
        let scope = self.parse_scope("iftrue")?;
        Ok(WhileNode{location, condition: expr, scope})
    }

    fn parse_expression(&mut self, stop_op: OperatorType) -> CompilerResult<Box<dyn ExpressionNode<'a, 'a> + 'a>> {
        self.parse_expression_until_one_of(&[stop_op])
    }

    fn parse_expression_until_one_of(&mut self, stop_ops: &[OperatorType]) -> CompilerResult<Box<dyn ExpressionNode<'a, 'a> + 'a>> {
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
                    } else if let Some(binary_op) = BinaryOperation::from_op_type(*op_type) {
                        builder.push_op(binary_op)?;
                    } else {
                        compiler_err!(token.location, "unexpected operator {:?}", op_type);
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
                    } else {
                        builder.push_expr(IdentifierNode{location: token.location, name: value.to_string()});
                    }
                },
                TokenType::Keyword(kw) => {
                    compiler_err!(token.location, "unexpected keyword {:?}", kw)
                },
            }
        }

        return builder.build();
    }

    fn parse_function_call(&mut self, location: Location, name: &str) -> CompilerResult<FunctionCall<'a, 'a>> {
        let mut args = Vec::new();

        let peek = self.reader.peek()?;
        if peek.token_type == TokenType::Operator(OperatorType::RightParen) {
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
}
