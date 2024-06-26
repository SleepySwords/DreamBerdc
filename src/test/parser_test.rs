use itertools::Itertools;

use crate::{lexer::Lexer, parser::Parser, TokenKind};
use std::{error::Error, fs};

#[cfg(test)]
use pretty_assertions::assert_eq;

#[cfg(test)]
fn test_parsing(filename: &str) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(String::from("./test/source/") + filename + ".db")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    let mut parser = Parser { tokens, pos: 0 };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_top_level_declaration() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                return Ok(());
            }
        };
        statements.push(function);
    }
    let expected = fs::read_to_string(String::from("./test/parsed/") + filename + ".parsed")?;

    assert_eq!(format!("{:#?}", statements), expected.trim());

    Ok(())
}

#[test]
fn test_parse_add() -> Result<(), Box<dyn Error>> {
    test_parsing("add")
}

#[test]
fn test_parse_array() -> Result<(), Box<dyn Error>> {
    test_parsing("array")
}

#[test]
fn test_parse_class() -> Result<(), Box<dyn Error>> {
    test_parsing("class")
}
