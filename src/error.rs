use crate::token::Location;
use inkwell::builder::BuilderError;

#[derive(Debug)]
pub struct CompilerError {
    pub message: String,
    pub location: Location,
}
pub type CompilerResult<T> = Result<T, CompilerError>;

pub trait CompilerResultErrorMapper {
    type Value;

    fn to_comp_res(self, loc: Location) -> CompilerResult<Self::Value>;
}

pub trait CompilerResultErrorMapperWithDesc {
    type Value;

    fn to_comp_res_with_desc(self, loc: Location, desc: &str) -> CompilerResult<Self::Value>;
}

pub enum SymbolLookupError {
    AlreadyRegistered(String),
    NoSymbolFound(String),
    NotANamespace,
}

impl SymbolLookupError {
    pub fn message(&self) -> String {
        match self {
            Self::AlreadyRegistered(sym_name) => {
                format!("symbol {} is already registered in this scope", sym_name)
            }
            Self::NoSymbolFound(sym_name) => format!("symbol not found: {}", sym_name),
            Self::NotANamespace => "symbol is not a namespace".into(),
        }
    }
}

pub type SymbolLookupResult<T> = Result<T, SymbolLookupError>;

impl<T> CompilerResultErrorMapper for SymbolLookupResult<T> {
    type Value = T;

    fn to_comp_res(self, loc: Location) -> CompilerResult<Self::Value> {
        self.map_err(|err| CompilerError {
            message: err.message(),
            location: loc,
        })
    }
}

#[macro_export]
macro_rules! compiler_err {
    ($loc:expr, $($args:expr), *) => {{
        return Err($crate::error::CompilerError{location: $loc, message: format!($($args), *)});
    }}
}

pub fn wrap_option<T>(loc: Location, res: Option<T>, msg: &str) -> CompilerResult<T> {
    if res.is_none() {
        println!("ERROR!");
    }
    match res {
        Some(value) => Ok(value),
        None => {
            compiler_err!(loc, "{}", msg);
        }
    }
}

impl<T> CompilerResultErrorMapper for Result<T, BuilderError> {
    type Value = T;

    fn to_comp_res(self, loc: Location) -> CompilerResult<Self::Value> {
        self.map_err(|be| CompilerError {
            message: format!("builder error: {:?}", be),
            location: loc,
        })
    }
}

impl<T> CompilerResultErrorMapperWithDesc for Option<T> {
    type Value = T;

    fn to_comp_res_with_desc(self, loc: Location, desc: &str) -> CompilerResult<Self::Value> {
        match self {
            Some(value) => Ok(value),
            None => {
                compiler_err!(loc, "{}", desc);
            }
        }
    }
}
