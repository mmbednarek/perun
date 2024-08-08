use std::io::Read;
use std::mem::take;
use crate::token::{Location, Token, TokenType, read_operator_type};

fn is_wide_space(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        '\r' => true,
        '\n' => true,
        _ => false,
    }
}

pub struct Lexer {
    reader: Box<dyn Read>,
    out_tokens: Vec<Token>,
    current_token: String,
    current_location: Location,
}

impl Lexer {
    pub fn new(reader: Box<dyn Read>) -> Lexer {
        Lexer{
            reader,
            out_tokens: Vec::new(),
            current_token: String::new(),
            current_location: Location{line: 1, column: 1},
        }
    }

    fn push_token(&mut self, token: TokenType) {
        self.out_tokens.push(Token{token_type: token, location: self.current_location.clone()});
    }

    fn next_char(&mut self) -> std::io::Result<u8> {
        let mut ch_buff: [u8; 1] = [0];
        self.reader.read_exact(&mut ch_buff)?;
        if ch_buff[0] == b'\n' {
            self.current_location.line += 1;
            self.current_location.column = 1;
        } else {
            self.current_location.column += 1;
        }
        Ok(ch_buff[0])
    }

    fn handle_identifier(&mut self) {
        if self.current_token.is_empty() {
            return;
        }

        // Try to parse it as integer.
        let ident_int = self.current_token.parse::<u64>();
        if let Ok(num) = ident_int {
            self.push_token(TokenType::Number(num));
            self.current_token.clear();
            return;
        }

        let identifier_tkn = TokenType::Identifier(take(&mut self.current_token));
        self.push_token(identifier_tkn);
    }

    fn handle_char(&mut self, ch: char) {
        if is_wide_space(ch) {
            self.handle_identifier();
            return 
        }

        if let Some(op) = read_operator_type(ch) {
            self.handle_identifier();
            self.push_token(TokenType::Operator(op));
            return 
        }

        self.current_token.push(ch as char);
    }

    pub fn read_tokens(&mut self) {
        loop {
            let res_ch = self.next_char();
            match res_ch {
                Ok(ch) => self.handle_char(ch as char),
                Err(_) => {
                    self.handle_identifier();
                    return;
                },
            }
        }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.out_tokens
    }
}
