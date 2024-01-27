use std::fmt::Display;

use colored::Colorize;
use inkwell::builder::BuilderError;
use thiserror::Error;

// FIXME: maybe use an &str
#[derive(Debug, Error)]
pub enum CompilerError {
    SyntaxError((usize, usize), String),
    CodeGenError(String),
    CodeGenErrorWithPos((usize, usize), String),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::SyntaxError((col, lnum), msg) => {
                write!(
                    f,
                    "{} {}",
                    format!(
                        "error({}, {}):",
                        (lnum + 1).to_string().white(),
                        (col + 1).to_string().white()
                    )
                    .bold()
                    .red(),
                    format!("Syntax Error: {}", msg).bold(),
                )
            }
            CompilerError::CodeGenError(msg) => {
                write!(
                    f,
                    "{} {}",
                    "error:".to_string().bold().red(),
                    format!("Compile Error: {}", msg).bold(),
                )
            }
            CompilerError::CodeGenErrorWithPos((col, lnum), msg) => {
                write!(
                    f,
                    "{} {}",
                    format!(
                        "error({}, {}):",
                        (lnum + 1).to_string().white(),
                        (col + 1).to_string().white()
                    )
                    .bold()
                    .red(),
                    format!("Compile Error: {}", msg).bold(),
                )
            }
        }
    }
}

impl From<BuilderError> for CompilerError {
    fn from(value: BuilderError) -> Self {
        return CompilerError::CodeGenError(format!("Build error: {}", value))
    }
}
