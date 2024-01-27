use itertools::Itertools;

use crate::{
    ast::{
        Declaration, ExpressionKind, ForStatement, Function, Operation, Prototype, StatementKind,
    },
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
            StatementKind::Function(Function {
                prototype: Prototype {
                    name: "add".to_string(),
                    arguments: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                    return_type: Type::Int
                },
                body: vec![StatementKind::Return {
                    return_value: ExpressionKind::Binary {
                        lhs: Box::new(ExpressionKind::Identifier("a".to_string())),
                        operation: Operation::Add,
                        rhs: Box::new(ExpressionKind::Identifier("b".to_string()))
                    }
                }]
            }),
            StatementKind::Function(Function {
                prototype: Prototype {
                    name: "main".to_string(),
                    arguments: vec![],
                    return_type: Type::Void
                },
                body: vec![StatementKind::Return {
                    return_value: ExpressionKind::Call {
                        callee: "add".to_string(),
                        arguments: vec![
                            ExpressionKind::Identifier("1".to_string()),
                            ExpressionKind::Identifier("5".to_string())
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
            StatementKind::Function(Function {
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
                    StatementKind::Declaration(Declaration {
                        mutable: Mutable::ALL,
                        lhs: String::from("d"),
                        rhs: ExpressionKind::Identifier(String::from("0"),),
                    },),
                    StatementKind::For(Box::new(ForStatement {
                        initialiser: StatementKind::Declaration(Declaration {
                            mutable: Mutable::ALL,
                            lhs: String::from("i"),
                            rhs: ExpressionKind::Identifier(String::from("0"),),
                        }),
                        condition: ExpressionKind::Binary {
                            lhs: Box::new(ExpressionKind::Identifier(String::from("i"))),
                            operation: Operation::Less,
                            rhs: Box::new(ExpressionKind::Identifier(String::from("10"))),
                        },
                        accumalator: ExpressionKind::Identifier(String::from("0"),),
                        body: Some(vec![StatementKind::Expression(
                            ExpressionKind::Assignment {
                                lhs: String::from("d"),
                                rhs: Box::new(ExpressionKind::Binary {
                                    lhs: Box::new(ExpressionKind::Identifier(String::from("d"))),
                                    operation: Operation::Add,
                                    rhs: Box::new(ExpressionKind::Identifier(String::from("i"))),
                                }),
                            }
                        )]),
                    })),
                    StatementKind::Expression(ExpressionKind::Call {
                        callee: String::from("putchar"),
                        arguments: vec![ExpressionKind::Binary {
                            lhs: Box::new(ExpressionKind::Identifier(String::from("d"))),
                            operation: Operation::Add,
                            rhs: Box::new(ExpressionKind::Identifier(String::from("65"))),
                        },],
                    },),
                    StatementKind::Return {
                        return_value: ExpressionKind::Identifier(String::from("d"),),
                    },
                ],
            },),
            StatementKind::Function(Function {
                prototype: Prototype {
                    name: String::from("add"),
                    arguments: vec![
                        (String::from("a"), Type::Int),
                        (String::from("b"), Type::Int)
                    ],
                    return_type: Type::Void,
                },
                body: vec![StatementKind::Return {
                    return_value: ExpressionKind::Binary {
                        lhs: Box::new(ExpressionKind::Binary {
                            lhs: Box::new(ExpressionKind::Identifier(String::from("a"))),
                            operation: Operation::Multiply,
                            rhs: Box::new(ExpressionKind::Identifier(String::from("b"))),
                        }),
                        operation: Operation::Add,
                        rhs: Box::new(ExpressionKind::Identifier(String::from("a"))),
                    },
                },],
            },),
            StatementKind::Function(Function {
                prototype: Prototype {
                    name: String::from("main"),
                    arguments: vec![
                        (String::from("a"), Type::Int,),
                        (String::from("b"), Type::Int,),
                    ],
                    return_type: Type::Int,
                },
                body: vec![StatementKind::Return {
                    return_value: ExpressionKind::Call {
                        callee: String::from("max"),
                        arguments: vec![
                            ExpressionKind::Identifier(String::from("1"),),
                            ExpressionKind::Identifier(String::from("2"),),
                            ExpressionKind::Identifier(String::from("3"),),
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
