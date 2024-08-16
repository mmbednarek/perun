use std::io::Read;
use std::mem::take;
use crate::token::{Location, OperatorType, Token, TokenType, Keyword};

fn is_wide_space(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        '\r' => true,
        '\n' => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq, Eq)]
enum LexState {
    Global,
    PreComment,
    CommentSingleLine,
}

pub struct Lexer {
    reader: Box<dyn Read>,
    out_tokens: Vec<Token>,
    current_token: String,
    state: LexState,
    token_start: Location,
    current_location: Location,
}

impl Lexer {
    pub fn new(reader: Box<dyn Read>) -> Lexer {
        Lexer{
            reader,
            out_tokens: Vec::new(),
            current_token: String::new(),
            state: LexState::Global,
            token_start: Location{line: 1, column: 1},
            current_location: Location{line: 1, column: 1},
        }
    }

    fn push_token(&mut self, token: TokenType) {
        self.out_tokens.push(Token{token_type: token, location: self.token_start});
        self.token_start = self.current_location;
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
            self.token_start = self.current_location;
            return;
        }

        // Check if it's a keyword
        if let Some(kw) = Keyword::from_string(self.current_token.as_ref()) {
            self.push_token(TokenType::Keyword(kw));
            self.current_token.clear();
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
        match self.state {
            LexState::PreComment => {
                if ch == '/' {
                    self.state = LexState::CommentSingleLine;
                } else {
                    self.state = LexState::Global;
                    self.push_token(TokenType::Operator(OperatorType::Slash));
                }
            },
            LexState::CommentSingleLine => {
                if ch == '\n' {
                    self.state = LexState::Global;
                }
            },
            LexState::Global => {
                if is_wide_space(ch) {
                    self.handle_identifier();
                    return 
                }

                if ch == '/' {
                    self.handle_identifier();
                    self.state = LexState::PreComment;
                    return
                }

                if let Some(op) = OperatorType::from_char(ch) {
                    self.handle_identifier();
                    self.push_token(TokenType::Operator(op));
                    return 
                }

                self.current_token.push(ch);
            },
        }

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
