use crate::token_reader::TokenReader;
use crate::token::{TokenType, OperatorType, Location, Keyword};
use crate::error::CompilerResult;
use crate::ast::*;
use crate::typing::Type;
use std::mem::take;

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
                    self.reader.expect_token(TokenType::Operator(OperatorType::Colon))?;
                    let var_type = self.parse_type()?;
                    self.reader.expect_token(TokenType::Operator(OperatorType::Equals))?;
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
        let mut expr_stack: Vec<Option<Box<dyn ExpressionNode>>> = Vec::new();
        let mut op_stack: Vec<OperatorType> = Vec::new();

        let peek = self.reader.peek();
        let token_start_loc = match peek {
            Ok(tkn) => tkn.location,
            Err(err) => err.location,
        };

        loop {
            let token = self.reader.next()?;
            match &token.token_type {
                TokenType::Operator(op_type) => {
                    if *op_type == stop_op {
                        break;
                    } else if *op_type == OperatorType::LeftParen {
                        expr_stack.push(Some(self.parse_expression(OperatorType::RightParen)?));
                    } else {
                        op_stack.push(*op_type);
                    }
                },
                TokenType::Number(num) => {
                    expr_stack.push(Some(Box::new(NumberNode{location: token.location, number: *num})));
                },
                TokenType::Identifier(value) => {
                    expr_stack.push(Some(Box::new(IdentifierNode{location: token.location, name: value.to_string()})));
                },
                TokenType::Keyword(kw) => {
                    compiler_err!(token.location, "unexpected keyword {:?}", kw)
                },
            }
        }

        expr_stack.reverse();
        op_stack.reverse();

        while !op_stack.is_empty() {
            let op_res = op_stack.pop();
            if op_res.is_none() {
                break
            }

            let op = op_res.unwrap();

            let left_opt = expr_stack.pop();
            let right_opt = expr_stack.pop();
            if right_opt.is_none() || left_opt.is_none() {
                compiler_err!(token_start_loc, "invalid expression");
            }

            let left = left_opt.unwrap().unwrap();
            let right = right_opt.unwrap().unwrap();

            let location = *left.get_location();

            match op {
                OperatorType::Plus => {expr_stack.push(Some(Box::new(PlusNode{location, left, right})));}
                OperatorType::Minus => {expr_stack.push(Some(Box::new(MinusNode{location, left, right})));}
                OperatorType::Asterisk => {expr_stack.push(Some(Box::new(MultNode{location, left, right})));}
                OperatorType::Slash => {expr_stack.push(Some(Box::new(DivNode{location, left, right})));}
                OperatorType::Less => {expr_stack.push(Some(Box::new(LessNode{location, left, right})));}
                OperatorType::Equals => {expr_stack.push(Some(Box::new(AssignNode{location, left, right})));}
                _ => compiler_err!(location, "invalid expression"),
            }
        }

        if expr_stack.len() != 1 {
            compiler_err!(token_start_loc, "invalid expression");
        }

        let expr = take(&mut expr_stack[0]);
        match expr {
            Some(expr_value) => Ok(expr_value),
            None => compiler_err!(token_start_loc, "invalid expression"),
        }
    }
}
