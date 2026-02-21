use crate::token::Location;
use inkwell::builder::BuilderError;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

pub enum AnyError {
    CompilerError(CompilerError),
    SymbolLookupError(SymbolLookupError),
    FileError(String),
    SerdeError(String),
    LLVMError(String),
}

impl Into<AnyError> for &CompilerError {
    fn into(self) -> AnyError {
        AnyError::CompilerError(self.clone())
    }
}

impl Into<AnyError> for &SymbolLookupError {
    fn into(self) -> AnyError {
        AnyError::SymbolLookupError(self.clone())
    }
}

impl Into<AnyError> for &std::io::Error {
    fn into(self) -> AnyError {
        AnyError::FileError(self.to_string())
    }
}

impl Into<AnyError> for &serde_json::Error {
    fn into(self) -> AnyError {
        AnyError::SerdeError(self.to_string())
    }
}

impl Into<AnyError> for &BuilderError {
    fn into(self) -> AnyError {
        AnyError::LLVMError(self.to_string())
    }
}

impl Into<AnyError> for &inkwell::support::LLVMString {
    fn into(self) -> AnyError {
        AnyError::LLVMError(self.to_string())
    }
}

impl Display for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyError::CompilerError(ce) => {
                write!(
                    f,
                    "Failed to compile source file [{}]: {}",
                    ce.location, ce.message
                )
            }
            AnyError::SymbolLookupError(sym) => {
                write!(f, "Failed to lookup symbol file: {}", sym.message())
            }
            AnyError::FileError(err) => {
                write!(f, "Failed to open file: {}", err)
            }
            AnyError::SerdeError(err) => {
                write!(f, "Failed encode/decode JSON file: {}", err)
            }
            AnyError::LLVMError(err) => {
                write!(f, "Failed to compile LLVM module: {}", err)
            }
        }
    }
}

pub type AnyResult<T> = Result<T, AnyError>;

pub fn expect(condition: bool, location: Location, message: String) -> CompilerResult<()> {
    if condition {
        Ok(())
    } else {
        Err(CompilerError{message, location})
    }
}
#[macro_export]
macro_rules! compiler_expect {
    ($cond:expr, $loc:expr, $($args:expr), *) => {{
        $crate::error::expect($cond, $loc, format!($($args), *))?
    }}
}
