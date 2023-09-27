use itertools::Itertools;

use crate::{TokenKind, lexer::Lexer};
use std::{error::Error, fs, vec};

#[test]
fn test_tokenize() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./test_files/add.drmbrd")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    assert_eq!(
        tokens,
        vec![
            TokenKind::Function,
            TokenKind::Symbol(String::from("add")),
            TokenKind::OpenPar,
            TokenKind::Symbol(String::from("a")),
            TokenKind::Colon,
            TokenKind::Symbol(String::from("int")),
            TokenKind::Comma,
            TokenKind::Symbol(String::from("b")),
            TokenKind::Colon,
            TokenKind::Symbol(String::from("int")),
            TokenKind::ClosePar,
            TokenKind::Arrow,
            TokenKind::OpenCurB,
            TokenKind::Return,
            TokenKind::Symbol(String::from("a")),
            TokenKind::Plus,
            TokenKind::Symbol(String::from("b")),
            TokenKind::Bang,
            TokenKind::CloseCurB,
            TokenKind::Function,
            TokenKind::Symbol(String::from("main")),
            TokenKind::OpenPar,
            TokenKind::ClosePar,
            TokenKind::OpenCurB,
            TokenKind::Return,
            TokenKind::Symbol(String::from("add")),
            TokenKind::OpenPar,
            TokenKind::Symbol(String::from("1")),
            TokenKind::Comma,
            TokenKind::Symbol(String::from("5")),
            TokenKind::ClosePar,
            TokenKind::Bang,
            TokenKind::CloseCurB,
            TokenKind::Eof
        ]
    );
    Ok(())
}
