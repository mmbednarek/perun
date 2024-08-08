#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorType {
    Plus,
    Minus,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Dot,
    Comma,
    Colon,
    Semicolon,
    Equals,
    Asterisk,
    Slash,
    Less,
    Greater,
}

pub fn read_operator_type(ch: char) -> Option<OperatorType> {
    match ch {
        '+' => Some(OperatorType::Plus),
        '-' => Some(OperatorType::Minus),
        '(' => Some(OperatorType::LeftParen),
        ')' => Some(OperatorType::RightParen),
        '{' => Some(OperatorType::LeftBrace),
        '}' => Some(OperatorType::RightBrace),
        '.' => Some(OperatorType::Dot),
        ',' => Some(OperatorType::Comma),
        ':' => Some(OperatorType::Colon),
        ';' => Some(OperatorType::Semicolon),
        '=' => Some(OperatorType::Equals),
        '*' => Some(OperatorType::Asterisk),
        '/' => Some(OperatorType::Slash),
        '<' => Some(OperatorType::Less),
        '>' => Some(OperatorType::Greater),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Identifier(String),
    Number(u64),
    Operator(OperatorType),
}

pub struct Token {
    pub token_type: TokenType,
    pub location: Location,
}

