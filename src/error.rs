use crate::token::Location;
use inkwell::builder::BuilderError;

#[derive(Debug)]
pub struct CompilerError {
    pub message: String,
    pub location: Location,
}
pub type CompilerResult<T> = Result<T, CompilerError>;

pub enum SymbolLookupError {
    FailedToRegisterSymbol,
    NoSymbolFound(String),
}

impl SymbolLookupError {
    pub fn message(&self) -> String {
        match self {
            Self::FailedToRegisterSymbol => "failed to register symbol".to_string(),
            Self::NoSymbolFound(sym_name) => format!("symbol not found: {}", sym_name),
        }
    }
}

pub type SymbolLookupResult<T> = Result<T, SymbolLookupError>;

pub fn err_with_location<T>(loc: Location, res: SymbolLookupResult<T>) -> CompilerResult<T> {
    res.map_err(|err| CompilerError{message: err.message(), location: loc})
}

#[macro_export]
macro_rules! compiler_err {
    ($loc:expr, $($args:expr), *) => {{
        return Err($crate::error::CompilerError{location: $loc, message: format!($($args), *)});
    }}
}

pub fn wrap_option<T>(loc: Location, res: Option<T>, msg: &str)  -> CompilerResult<T> {
    match res {
        Some(value) => Ok(value),
        None => compiler_err!(loc, "{}", msg),
    }
}


pub fn wrap_err<T>(loc: Location, res: Result<T, BuilderError>)  -> CompilerResult<T> {
    res.map_err(|be| CompilerError{message: format!("builder error: {:?}", be), location: loc})
}
