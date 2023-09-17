use std::fmt::Display;

use clap::{Parser, ValueEnum};


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Mode{
    Jit,
    LLVMIR,
    Object,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Mode::Jit => "jit",
            Mode::LLVMIR => "llvmir",
            Mode::Object => "object",
        })
    }
}

#[derive(Parser, Debug)]
#[command(name = "MyApp")]
#[command(version = "1.0")]
pub struct Args {
    pub input: String,

    #[arg(short, long)]
    pub output: Option<String>,

    #[arg(default_value_t = Mode::Jit)]
    #[arg(short, long)]
    pub mode: Mode,
}
