use std::fmt::Display;

use colored::Colorize;
use thiserror::Error;

// FIXME: maybe use an &str
#[derive(Debug, Error)]
pub enum CompilerError {
    SyntaxError((usize, usize), String),
    CodeGenError(String),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::SyntaxError((col, lnum), msg) => {
                write!(
                    f,
                    "{} {}",
                    format!(
                        "ERROR({}, {}):",
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
                    "ERROR:".to_string().bold().red(),
                    format!("Compile Error: {}", msg).bold(),
                )
            }
        }
    }
}
