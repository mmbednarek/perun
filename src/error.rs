use crate::token::Location;

#[derive(Debug)]
pub struct CompilerError {
    pub message: String,
    pub location: Location,
}
pub type CompilerResult<T> = Result<T, CompilerError>;

#[macro_export]
macro_rules! compiler_err {
    ($loc:expr, $($args:expr), *) => {{
        return Err($crate::error::CompilerError{location: $loc, message: format!($($args), *)});
    }}
}
