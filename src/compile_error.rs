use std::fmt::Display;

use colored::Colorize;
use inkwell::builder::BuilderError;
use thiserror::Error;

use crate::ast::SourcePosition;

// FIXME: maybe use an &str
#[derive(Debug, Error)]
pub enum CompilerError {
    SyntaxError(SourcePosition, String),
    BuilderError(String),
    CodeGenError(SourcePosition, String),
}

impl CompilerError {
    pub fn code_gen_error<T: Into<String>>(pos: SourcePosition, str: T) -> Self {
        Self::CodeGenError(pos, str.into())
    }

    pub fn syntax_error<T: Into<String>>(pos: SourcePosition, str: T) -> Self {
        Self::CodeGenError(pos, str.into())
    }
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
            CompilerError::BuilderError(msg) => {
                write!(
                    f,
                    "{} {}",
                    "error:".to_string().bold().red(),
                    format!("Compile Error: {}", msg).bold(),
                )
            }
            CompilerError::CodeGenError((col, lnum), msg) => {
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
        CompilerError::BuilderError(format!("Build error: {}", value))
    }
}
