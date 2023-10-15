use itertools::Itertools;

use crate::{
    ast::{Call, Expression, Function, Operation, Prototype, Statement},
    lexer::Lexer,
    parser::Parser,
    types::Type,
    TokenKind,
};
use std::{error::Error, fs};

#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_parse_addtokenize() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./test_files/add.db")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    let mut parser = Parser { tokens, pos: 0 };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_function() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                return Ok(());
            }
        };
        statements.push(function);
    }

    assert_eq!(
        statements,
        vec![
            Statement::Function(Box::new(Function {
                prototype: Prototype {
                    name: "add".to_string(),
                    arguments: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                    return_type: Type::Int
                },
                body: vec![Statement::Return {
                    return_value: Box::new(Expression::Binary {
                        lhs: Box::new(Expression::Identifier("a".to_string())),
                        operation: Operation::Add,
                        rhs: Box::new(Expression::Identifier("b".to_string()))
                    })
                }]
            })),
            Statement::Function(Box::new(Function {
                prototype: Prototype {
                    name: "main".to_string(),
                    arguments: vec![],
                    return_type: Type::Void
                },
                body: vec![Statement::Return {
                    return_value: Box::new(Expression::Call(Call {
                        callee: "add".to_string(),
                        arguments: vec![
                            Expression::Identifier("1".to_string()),
                            Expression::Identifier("5".to_string())
                        ]
                    }))
                }]
            }))
        ]
    );
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}

#[test]
fn test_parse_for() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./test_files/for_loop.db")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    let mut parser = Parser { tokens, pos: 0 };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_function() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                return Ok(());
            }
        };
        statements.push(function);
    }

    assert_eq!(statements, vec![]);
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
