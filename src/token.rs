use std::fs::OpenOptions;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    LeftSquare,
    RightSquare,
    Dot,
    Comma,
    Colon,
    Semicolon,
    Equals,
    Asterisk,
    Slash,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    EqualsEquals,
    NotEquals,
    Ampersand,
    Percent,
    Not,
}

impl OperatorType {
    pub fn from_char(ch: char) -> Option<OperatorType> {
        match ch {
            '+' => Some(OperatorType::Plus),
            '-' => Some(OperatorType::Minus),
            '(' => Some(OperatorType::LeftParen),
            ')' => Some(OperatorType::RightParen),
            '{' => Some(OperatorType::LeftBrace),
            '}' => Some(OperatorType::RightBrace),
            '[' => Some(OperatorType::LeftSquare),
            ']' => Some(OperatorType::RightSquare),
            '.' => Some(OperatorType::Dot),
            ',' => Some(OperatorType::Comma),
            ':' => Some(OperatorType::Colon),
            ';' => Some(OperatorType::Semicolon),
            '=' => Some(OperatorType::Equals),
            '*' => Some(OperatorType::Asterisk),
            '/' => Some(OperatorType::Slash),
            '<' => Some(OperatorType::Less),
            '>' => Some(OperatorType::Greater),
            '&' => Some(OperatorType::Ampersand),
            '%' => Some(OperatorType::Percent),
            '!' => Some(OperatorType::Not),
            _ => None,
        }
    }

    pub fn has_continuation(&self) -> bool {
        match self {
            OperatorType::Equals => true,
            OperatorType::Less => true,
            OperatorType::Greater => true,
            OperatorType::Not => true,
            _ => false,
        }
    }

    pub fn join(&self, other: &OperatorType) -> Option<OperatorType> {
        match self {
            OperatorType::Equals => match other {
                OperatorType::Equals => Some(OperatorType::EqualsEquals),
                _ => None,
            },
            OperatorType::Less => match other {
                OperatorType::Equals => Some(OperatorType::LessOrEqual),
                _ => None,
            },
            OperatorType::Greater => match other {
                OperatorType::Equals => Some(OperatorType::GreaterOrEqual),
                _ => None,
            },
            OperatorType::Not => match other {
                OperatorType::Equals => Some(OperatorType::NotEquals),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    Var,
    Return,
    Fn,
    If,
    While,
    Extern,
    Void,
    RawPtr,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64
}

impl Keyword {
    pub fn from_string(value: &str) -> Option<Keyword> {
        match value {
            "void" => Some(Keyword::Void),
            "rawptr" => Some(Keyword::RawPtr),
            "var" => Some(Keyword::Var),
            "return" => Some(Keyword::Return),
            "fn" => Some(Keyword::Fn),
            "if" => Some(Keyword::If),
            "while" => Some(Keyword::While),
            "extern" => Some(Keyword::Extern),
            "i8" => Some(Keyword::Int8),
            "i16" => Some(Keyword::Int16),
            "i32" => Some(Keyword::Int32),
            "i64" => Some(Keyword::Int64),
            "f32" => Some(Keyword::Float32),
            "f64" => Some(Keyword::Float64),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Identifier(String),
    Number(u64),
    Operator(OperatorType),
    Keyword(Keyword),
    String(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub location: Location,
}

