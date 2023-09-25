use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Syntax Error: {0}")]
    SyntaxError(String),
}
