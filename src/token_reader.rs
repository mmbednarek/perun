use crate::error::CompilerResult;
use crate::token::{Token, TokenType, Location, Keyword};

pub struct TokenReader<'a> {
    tokens: &'a [Token],
    at: usize,
}

impl<'a> TokenReader<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self{tokens, at: 0}
    }

    pub fn has_tokens(&self) -> bool {
        self.at < self.tokens.len()
    }

    pub fn next(&mut self) -> CompilerResult<&Token> {
        let index = self.at;
        if index >= self.tokens.len() {
            compiler_err!(Location{line: 1, column: 1}, "unexpected and of stream");
        }

        self.at += 1;
        Ok(&self.tokens[index])
    }

    pub fn seek_back(&mut self) {
        if self.at != 0 {
            self.at -= 1;
        }
    }

    pub fn peek(&self) -> CompilerResult<&Token> {
        if self.at >= self.tokens.len() {
            compiler_err!(Location{line: 1, column: 1}, "unexpected and of stream");
        }
        Ok(&self.tokens[self.at])
    }

    pub fn expect_token(&mut self, token_type: TokenType) -> CompilerResult<&Token> {
        let token = self.next()?;
        if token.token_type != token_type {
            compiler_err!(token.location, "invalid token type {:?}, expected {:?}", token.token_type, token_type);
        }

        Ok(token)
    }

    pub fn expect_identifier(&mut self) -> CompilerResult<String> {
        let (_, iden) = self.expect_identifier_with_loc()?;
        Ok(iden)
    }

    pub fn expect_identifier_with_loc(&mut self) -> CompilerResult<(Location, String)> {
        let token = self.next()?;
        if let TokenType::Identifier(identifier) = &token.token_type {
            return Ok((token.location, identifier.clone()));
        }

        compiler_err!(token.location, "invalid token type {:?}, expected Identifier", token.token_type)
    }

    pub fn skip_token_if_present(&mut self, token_to_skip: TokenType) -> CompilerResult<bool> {
        let token = self.peek()?;
        if token.token_type == token_to_skip {
            self.next()?;
            return Ok(true);
        }

        Ok(false)
    }

    pub fn expect_keyword(&mut self) -> CompilerResult<Keyword> {
        let token = self.next()?;
        if let TokenType::Keyword(kw) = &token.token_type {
            return Ok(kw.clone());
        }

        compiler_err!(token.location, "invalid token type {:?}, expected Keyword", token.token_type)
    }

    pub fn expect_keyword_with_loc(&mut self) -> CompilerResult<(Location, Keyword)> {
        let token = self.next()?;
        if let TokenType::Keyword(kw) = &token.token_type {
            return Ok((token.location.clone(), kw.clone()));
        }

        compiler_err!(token.location, "invalid token type {:?}, expected Keyword", token.token_type)
    }
}
