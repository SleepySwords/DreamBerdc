use std::fmt::Display;

use colored::Colorize;

#[derive(Debug)]
pub enum CompileError {
    SyntaxError((usize, usize), String),
    CompileError(String),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::SyntaxError((col, lnum), msg) => {
                write!(
                    f,
                    "{} {}",
                    format!("ERROR({}, {}):", (lnum + 1).to_string().white(), (col + 1).to_string().white()).bold().red(),
                    format!("Syntax Error: {}", msg).bold(),
                )
            }
            CompileError::CompileError(msg) => {
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
