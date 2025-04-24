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
    DoubleColon,
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
    Or,
    LogicalOr,
    LogicalAnd,
    Increase,
    Decrease,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
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
            '|' => Some(OperatorType::Or),
            _ => None,
        }
    }

    pub fn has_continuation(&self) -> bool {
        match self {
            OperatorType::Equals => true,
            OperatorType::Less => true,
            OperatorType::Greater => true,
            OperatorType::Not => true,
            OperatorType::Ampersand => true,
            OperatorType::Or => true,
            OperatorType::Colon => true,
            OperatorType::Plus => true,
            OperatorType::Minus => true,
            OperatorType::Asterisk => true,
            OperatorType::Slash => true,
            OperatorType::Percent => true,
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
            OperatorType::Ampersand => match other {
                OperatorType::Ampersand => Some(OperatorType::LogicalAnd),
                _ => None,
            },
            OperatorType::Or => match other {
                OperatorType::Or => Some(OperatorType::LogicalOr),
                _ => None,
            },
            OperatorType::Colon => match other {
                OperatorType::Colon => Some(OperatorType::DoubleColon),
                _ => None,
            },
            OperatorType::Plus => match other {
                OperatorType::Equals => Some(OperatorType::Increase),
                _ => None,
            },
            OperatorType::Minus => match other {
                OperatorType::Equals => Some(OperatorType::Decrease),
                _ => None,
            },
            OperatorType::Asterisk => match other {
                OperatorType::Equals => Some(OperatorType::MultiplyAssign),
                _ => None,
            },
            OperatorType::Slash => match other {
                OperatorType::Equals => Some(OperatorType::DivideAssign),
                _ => None,
            },
            OperatorType::Percent => match other {
                OperatorType::Equals => Some(OperatorType::ModuloAssign),
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
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Bool,
    Const,
    Struct,
    Ref,
    Null,
    Else,
    SelfKw,
    Import,
    Public,
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
            "u8" => Some(Keyword::UInt8),
            "u16" => Some(Keyword::UInt16),
            "u32" => Some(Keyword::UInt32),
            "u64" => Some(Keyword::UInt64),
            "f32" => Some(Keyword::Float32),
            "f64" => Some(Keyword::Float64),
            "bool" => Some(Keyword::Bool),
            "const" => Some(Keyword::Const),
            "struct" => Some(Keyword::Struct),
            "ref" => Some(Keyword::Ref),
            "null" => Some(Keyword::Null),
            "else" => Some(Keyword::Else),
            "self" => Some(Keyword::SelfKw),
            "import" => Some(Keyword::Import),
            "pub" => Some(Keyword::Public),
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
