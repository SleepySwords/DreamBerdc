use itertools::Itertools;

use crate::{
    lexer::{Lexer, Token},
    TokenKind,
};
use std::{error::Error, fs, vec};

#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_tokenize() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./test_files/add.db")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Function, 0, 0),
            Token::new(TokenKind::Symbol(String::from("add")), 0, 9),
            Token::new(TokenKind::OpenPar, 0, 12),
            Token::new(TokenKind::Symbol(String::from("a")), 0, 13),
            Token::new(TokenKind::Colon, 0, 14),
            Token::new(TokenKind::Symbol(String::from("int")), 0, 16),
            Token::new(TokenKind::Comma, 0, 19),
            Token::new(TokenKind::Symbol(String::from("b")), 0, 21),
            Token::new(TokenKind::Colon, 0, 22),
            Token::new(TokenKind::Symbol(String::from("int")), 0, 24),
            Token::new(TokenKind::ClosePar, 0, 27),
            Token::new(TokenKind::Colon, 0, 28),
            Token::new(TokenKind::Symbol(String::from("int")), 0, 30),
            Token::new(TokenKind::Arrow, 0, 34),
            Token::new(TokenKind::OpenCurB, 0, 37),
            Token::new(TokenKind::If, 1, 4),
            Token::new(TokenKind::OpenPar, 1, 7),
            Token::new(TokenKind::Symbol(String::from("a")), 1, 8),
            Token::new(TokenKind::Gt, 1, 10),
            Token::new(TokenKind::Symbol(String::from("1")), 1, 12),
            Token::new(TokenKind::ClosePar, 1, 13),
            Token::new(TokenKind::OpenCurB, 1, 15),
            Token::new(TokenKind::Symbol(String::from("puts")), 2, 8),
            Token::new(TokenKind::OpenPar, 2, 12),
            Token::new(TokenKind::String(String::from("first")), 2, 13),
            Token::new(TokenKind::ClosePar, 2, 20),
            Token::new(TokenKind::CloseCurB, 3, 4),
            Token::new(TokenKind::Else, 3, 6),
            Token::new(TokenKind::OpenCurB, 3, 11),
            Token::new(TokenKind::Symbol(String::from("puts")), 4, 8),
            Token::new(TokenKind::OpenPar, 4, 12),
            Token::new(TokenKind::String(String::from("second")), 4, 13),
            Token::new(TokenKind::ClosePar, 4, 21),
            Token::new(TokenKind::CloseCurB, 5, 4),
            Token::new(TokenKind::Return, 6, 4),
            Token::new(TokenKind::Symbol(String::from("a")), 6, 11),
            Token::new(TokenKind::Plus, 6, 13),
            Token::new(TokenKind::Symbol(String::from("b")), 6, 15),
            Token::new(TokenKind::Bang, 6, 16),
            Token::new(TokenKind::CloseCurB, 7, 0),
            Token::new(TokenKind::Function, 9, 0),
            Token::new(TokenKind::Symbol(String::from("main")), 9, 9),
            Token::new(TokenKind::OpenPar, 9, 13),
            Token::new(TokenKind::ClosePar, 9, 14),
            Token::new(TokenKind::Arrow, 9, 16),
            Token::new(TokenKind::OpenCurB, 9, 19),
            Token::new(TokenKind::Var, 10, 4),
            Token::new(TokenKind::Const, 10, 8),
            Token::new(TokenKind::Symbol(String::from("test")), 10, 14),
            Token::new(TokenKind::Eq, 10, 19),
            Token::new(TokenKind::Symbol(String::from("add")), 10, 21),
            Token::new(TokenKind::OpenPar, 10, 24),
            Token::new(TokenKind::Symbol(String::from("6")), 10, 25),
            Token::new(TokenKind::Comma, 10, 26),
            Token::new(TokenKind::Symbol(String::from("59")), 10, 28),
            Token::new(TokenKind::ClosePar, 10, 30),
            Token::new(TokenKind::Bang, 10, 31),
            Token::new(TokenKind::Symbol(String::from("test")), 11, 4),
            Token::new(TokenKind::Eq, 11, 9),
            Token::new(TokenKind::Symbol(String::from("add")), 11, 11),
            Token::new(TokenKind::OpenPar, 11, 14),
            Token::new(TokenKind::Symbol(String::from("1")), 11, 15),
            Token::new(TokenKind::Comma, 11, 16),
            Token::new(TokenKind::Symbol(String::from("64")), 11, 18),
            Token::new(TokenKind::ClosePar, 11, 20),
            Token::new(TokenKind::Bang, 11, 21),
            Token::new(TokenKind::Symbol(String::from("putchar")), 12, 4),
            Token::new(TokenKind::OpenPar, 12, 11),
            Token::new(TokenKind::Symbol(String::from("test")), 12, 12),
            Token::new(TokenKind::ClosePar, 12, 16),
            Token::new(TokenKind::Symbol(String::from("putchar")), 13, 4),
            Token::new(TokenKind::OpenPar, 13, 11),
            Token::new(TokenKind::Symbol(String::from("add")), 13, 12),
            Token::new(TokenKind::OpenPar, 13, 15),
            Token::new(TokenKind::Symbol(String::from("1")), 13, 16),
            Token::new(TokenKind::Comma, 13, 17),
            Token::new(TokenKind::Symbol(String::from("2")), 13, 19),
            Token::new(TokenKind::Plus, 13, 21),
            Token::new(TokenKind::Symbol(String::from("7")), 13, 23),
            Token::new(TokenKind::ClosePar, 13, 24),
            Token::new(TokenKind::ClosePar, 13, 25),
            Token::new(TokenKind::Symbol(String::from("puts")), 14, 4),
            Token::new(TokenKind::OpenPar, 14, 8),
            Token::new(TokenKind::String(String::from("Hello world")), 14, 9),
            Token::new(TokenKind::ClosePar, 14, 22),
            Token::new(TokenKind::Bang, 14, 23),
            Token::new(TokenKind::Return, 15, 4),
            Token::new(TokenKind::Symbol(String::from("0")), 15, 11),
            Token::new(TokenKind::Bang, 15, 12),
            Token::new(TokenKind::CloseCurB, 16, 0),
            Token::new(TokenKind::Eof, 17, 0),
        ]
    );
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
