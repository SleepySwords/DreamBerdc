use std::fmt::Display;

use clap::{Parser, ValueEnum};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Mode {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Optimisation {
    None,
    Less,
    Default,
    Aggresive,
}

impl Display for Optimisation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Optimisation::None => "none",
            Optimisation::Less => "less",
            Optimisation::Default => "default",
            Optimisation::Aggresive => "aggresive",
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

    #[arg(short = 'O', long)]
    #[arg(default_value_t = Optimisation::None)]
    pub optimisation: Optimisation,
}
