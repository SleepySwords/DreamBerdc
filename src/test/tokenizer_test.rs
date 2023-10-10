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
    let contents = fs::read_to_string("./test_files/add.drmbrd")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Function, 0, 0),
            Token::new(TokenKind::Symbol("add".to_string()), 0, 8),
            Token::new(TokenKind::OpenPar, 0, 11),
            Token::new(TokenKind::Symbol("a".to_string()), 0, 12),
            Token::new(TokenKind::Colon, 0, 13),
            Token::new(TokenKind::Symbol("int".to_string()), 0, 15),
            Token::new(TokenKind::Comma, 0, 18),
            Token::new(TokenKind::Symbol("b".to_string()), 0, 20),
            Token::new(TokenKind::Colon, 0, 21),
            Token::new(TokenKind::Symbol("int".to_string()), 0, 23),
            Token::new(TokenKind::ClosePar, 0, 26),
            Token::new(TokenKind::Colon, 0, 27),
            Token::new(TokenKind::Symbol("int".to_string()), 0, 29),
            Token::new(TokenKind::Arrow, 0, 33),
            Token::new(TokenKind::OpenCurB, 0, 36),
            Token::new(TokenKind::Return, 1, 1),
            Token::new(TokenKind::Symbol("a".to_string()), 1, 8),
            Token::new(TokenKind::Plus, 1, 10),
            Token::new(TokenKind::Symbol("b".to_string()), 1, 12),
            Token::new(TokenKind::Bang, 1, 13),
            Token::new(TokenKind::CloseCurB, 2, 0),
            Token::new(TokenKind::Function, 4, 0),
            Token::new(TokenKind::Symbol("main".to_string()), 4, 9),
            Token::new(TokenKind::OpenPar, 4, 13),
            Token::new(TokenKind::ClosePar, 4, 14),
            Token::new(TokenKind::Arrow, 4, 16),
            Token::new(TokenKind::OpenCurB, 4, 19),
            Token::new(TokenKind::Return, 5, 1),
            Token::new(TokenKind::Symbol("add".to_string()), 5, 8),
            Token::new(TokenKind::OpenPar, 5, 11),
            Token::new(TokenKind::Symbol("1".to_string()), 5, 12),
            Token::new(TokenKind::Comma, 5, 13),
            Token::new(TokenKind::Symbol("5".to_string()), 5, 14),
            Token::new(TokenKind::ClosePar, 5, 15),
            Token::new(TokenKind::Bang, 5, 16),
            Token::new(TokenKind::CloseCurB, 6, 0),
            Token::new(TokenKind::Eof, 7, 0),
        ]
    );
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
