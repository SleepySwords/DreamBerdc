use itertools::Itertools;

use crate::{
    ast::{Declaration, Expression, ForStatement, Function, Operation, Prototype, Statement},
    lexer::Lexer,
    parser::Parser,
    types::Type,
    utils::Mutable,
    TokenKind,
};
use std::{error::Error, fs};

#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_parse_addtokenize() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./test_files_play/add.db")?;
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
            Statement::Function(Function {
                prototype: Prototype {
                    name: "add".to_string(),
                    arguments: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                    return_type: Type::Int
                },
                body: vec![Statement::Return {
                    return_value: Expression::Binary {
                        lhs: Box::new(Expression::Identifier("a".to_string())),
                        operation: Operation::Add,
                        rhs: Box::new(Expression::Identifier("b".to_string()))
                    }
                }]
            }),
            Statement::Function(Function {
                prototype: Prototype {
                    name: "main".to_string(),
                    arguments: vec![],
                    return_type: Type::Void
                },
                body: vec![Statement::Return {
                    return_value: Expression::Call {
                        callee: "add".to_string(),
                        arguments: vec![
                            Expression::Identifier("1".to_string()),
                            Expression::Identifier("5".to_string())
                        ]
                    }
                }]
            })
        ]
    );
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}

#[test]
fn test_parse_for() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./test_files_play/for_loop.db")?;
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
            Statement::Function(Function {
                prototype: Prototype {
                    name: String::from("max"),
                    arguments: vec![
                        (String::from("a"), Type::Int,),
                        (String::from("b"), Type::Int,),
                        (String::from("c"), Type::Int,),
                    ],
                    return_type: Type::Int,
                },
                body: vec![
                    Statement::Declaration(Declaration {
                        mutable: Mutable::ALL,
                        lhs: String::from("d"),
                        rhs: Expression::Identifier(String::from("0"),),
                    },),
                    Statement::For(Box::new(ForStatement {
                        initialiser: Statement::Declaration(Declaration {
                            mutable: Mutable::ALL,
                            lhs: String::from("i"),
                            rhs: Expression::Identifier(String::from("0"),),
                        }),
                        condition: Expression::Binary {
                            lhs: Box::new(Expression::Identifier(String::from("i"))),
                            operation: Operation::Less,
                            rhs: Box::new(Expression::Identifier(String::from("10"))),
                        },
                        accumalator: Expression::Identifier(String::from("0"),),
                        body: Some(vec![Statement::Expression(Expression::Assignment {
                            lhs: String::from("d"),
                            rhs: Box::new(Expression::Binary {
                                lhs: Box::new(Expression::Identifier(String::from("d"))),
                                operation: Operation::Add,
                                rhs: Box::new(Expression::Identifier(String::from("i"))),
                            }),
                        })]),
                    })),
                    Statement::Expression(Expression::Call {
                        callee: String::from("putchar"),
                        arguments: vec![Expression::Binary {
                            lhs: Box::new(Expression::Identifier(String::from("d"))),
                            operation: Operation::Add,
                            rhs: Box::new(Expression::Identifier(String::from("65"))),
                        },],
                    },),
                    Statement::Return {
                        return_value: Expression::Identifier(String::from("d"),),
                    },
                ],
            },),
            Statement::Function(Function {
                prototype: Prototype {
                    name: String::from("add"),
                    arguments: vec![
                        (String::from("a"), Type::Int),
                        (String::from("b"), Type::Int)
                    ],
                    return_type: Type::Void,
                },
                body: vec![Statement::Return {
                    return_value: Expression::Binary {
                        lhs: Box::new(Expression::Binary {
                            lhs: Box::new(Expression::Identifier(String::from("a"))),
                            operation: Operation::Multiply,
                            rhs: Box::new(Expression::Identifier(String::from("b"))),
                        }),
                        operation: Operation::Add,
                        rhs: Box::new(Expression::Identifier(String::from("a"))),
                    },
                },],
            },),
            Statement::Function(Function {
                prototype: Prototype {
                    name: String::from("main"),
                    arguments: vec![
                        (String::from("a"), Type::Int,),
                        (String::from("b"), Type::Int,),
                    ],
                    return_type: Type::Int,
                },
                body: vec![Statement::Return {
                    return_value: Expression::Call {
                        callee: String::from("max"),
                        arguments: vec![
                            Expression::Identifier(String::from("1"),),
                            Expression::Identifier(String::from("2"),),
                            Expression::Identifier(String::from("3"),),
                        ],
                    },
                },],
            },),
        ]
    );
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
