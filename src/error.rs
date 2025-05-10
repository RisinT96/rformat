use thiserror::Error;

pub type Result<T> = core::result::Result<T, FormatError>;

#[derive(Error, Debug, PartialEq)]
pub enum FormatError {
    #[error("Format error: {0}")]
    FmtError(#[from] std::fmt::Error),
    #[error("Unescaped closing brace without opening brace, at index {0} in '{1}'")]
    ClosingParentheses(usize, String),
    #[error("Requested more parameters than provided!")]
    NotEnoughParameters,
    #[error("Positional parameter requested ({0}) but not provided")]
    NoPositionalParameter(usize),
    #[error("Named parameter requested ({0}) but not provided")]
    NoNamedParameter(String),
    #[error("Failed to parse integer: {0}")]
    InvalidInteger(#[from] core::num::ParseIntError),
    #[error("Invalid format spec, expected format type in '{full}', found: '{substr}'")]
    InvalidFormatSpec { full: String, substr: String },
    #[error("Unsupported format type: '{0}'")]
    UnsupportedFormatType(String),
    #[error("Parameter passed as precision is not of type usize")]
    PrecisionNotUsize,
    #[error("Parameter passed as width is not of type usize")]
    WidthNotUsize,
}
