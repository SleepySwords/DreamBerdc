use itertools::Itertools;

use crate::{
    ast::{
        Declaration, Expression, ExpressionKind, ForStatement, Function, Operation, Prototype,
        Statement, StatementKind,
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
            Statement::from_pos(
                StatementKind::Function(Function {
                    prototype: Prototype {
                        name: "add".to_string(),
                        arguments: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                        return_type: Type::Int
                    },
                    body: vec![Statement::from_pos(
                        StatementKind::Return {
                            return_value: Expression::from_pos(
                                ExpressionKind::Binary {
                                    lhs: Box::new(Expression::from_pos(
                                        ExpressionKind::Identifier("a".to_string()),
                                        (11, 1)
                                    )),
                                    operation: Operation::Add,
                                    rhs: Box::new(Expression::from_pos(
                                        ExpressionKind::Identifier("b".to_string()),
                                        (15, 1)
                                    ))
                                },
                                (15, 1)
                            )
                        },
                        (4, 1)
                    )]
                }),
                (0, 0)
            ),
            Statement::from_pos(
                StatementKind::Function(Function {
                    prototype: Prototype {
                        name: "main".to_string(),
                        arguments: vec![],
                        return_type: Type::Void
                    },
                    body: vec![Statement::from_pos(
                        StatementKind::Return {
                            return_value: Expression::from_pos(
                                ExpressionKind::Call {
                                    callee: "add".to_string(),
                                    arguments: vec![
                                        Expression::from_pos(
                                            ExpressionKind::Identifier("1".to_string()),
                                            (15, 5)
                                        ),
                                        Expression::from_pos(
                                            ExpressionKind::Identifier("5".to_string()),
                                            (17, 5)
                                        ),
                                    ]
                                },
                                (11, 5)
                            )
                        },
                        (4, 5)
                    )]
                }),
                (0, 4)
            )
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
            Statement::from_pos(
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
                        Statement::from_pos(
                            StatementKind::Declaration(Declaration {
                                mutable: Mutable::ALL,
                                var_type: None,
                                lhs: String::from("d"),
                                rhs: Expression::from_pos(
                                    ExpressionKind::Identifier(String::from("0")),
                                    (16, 1)
                                ),
                            }),
                            (4, 1)
                        ),
                        Statement::from_pos(
                            StatementKind::For(Box::new(ForStatement {
                                initialiser: Statement::from_pos(
                                    StatementKind::Declaration(Declaration {
                                        mutable: Mutable::ALL,
                                        var_type: None,
                                        lhs: String::from("i"),
                                        rhs: Expression::from_pos(
                                            ExpressionKind::Identifier(String::from("0"),),
                                            (21, 2)
                                        ),
                                    }),
                                    (9, 2)
                                ),
                                condition: Expression::from_pos(
                                    ExpressionKind::Binary {
                                        lhs: Box::new(Expression::from_pos(
                                            ExpressionKind::Identifier(String::from("i")),
                                            (24, 2)
                                        )),
                                        operation: Operation::Less,
                                        rhs: Box::new(Expression::from_pos(
                                            ExpressionKind::Identifier(String::from("10")),
                                            (28, 2)
                                        )),
                                    },
                                    (28, 2)
                                ),
                                accumalator: Expression::from_pos(
                                    ExpressionKind::Identifier(String::from("0")),
                                    (32, 2)
                                ),
                                body: Some(vec![Statement::from_pos(
                                    StatementKind::Expression(Expression::from_pos(
                                        ExpressionKind::Assignment {
                                            lhs: String::from("d"),
                                            rhs: Box::new(Expression::from_pos(
                                                ExpressionKind::Binary {
                                                    lhs: Box::new(Expression::from_pos(
                                                        ExpressionKind::Identifier(String::from(
                                                            "d"
                                                        )),
                                                        (12, 3)
                                                    )),
                                                    operation: Operation::Add,
                                                    rhs: Box::new(Expression::from_pos(
                                                        ExpressionKind::Identifier(String::from(
                                                            "i"
                                                        )),
                                                        (16, 3)
                                                    )),
                                                },
                                                (16, 3)
                                            )),
                                        },
                                        (8, 3)
                                    )),
                                    (8, 3)
                                )]),
                            })),
                            (4, 2)
                        ),
                        Statement::from_pos(
                            StatementKind::Expression(Expression::from_pos(
                                ExpressionKind::Call {
                                    callee: String::from("putchar"),
                                    arguments: vec![Expression::from_pos(
                                        ExpressionKind::Binary {
                                            lhs: Box::new(Expression::from_pos(
                                                ExpressionKind::Identifier(String::from("d")),
                                                (12, 5)
                                            )),
                                            operation: Operation::Add,
                                            rhs: Box::new(Expression::from_pos(
                                                ExpressionKind::Identifier(String::from("65")),
                                                (14, 5)
                                            )),
                                        },
                                        (14, 5)
                                    )],
                                },
                                (4, 5)
                            )),
                            (4, 5)
                        ),
                        Statement::from_pos(
                            StatementKind::Return {
                                return_value: Expression::from_pos(
                                    ExpressionKind::Identifier(String::from("d")),
                                    (11, 6)
                                ),
                            },
                            (4, 6)
                        ),
                    ],
                }),
                (0, 0)
            ),
            Statement::from_pos(
                StatementKind::Function(Function {
                    prototype: Prototype {
                        name: String::from("add"),
                        arguments: vec![
                            (String::from("a"), Type::Int),
                            (String::from("b"), Type::Int)
                        ],
                        return_type: Type::Void,
                    },
                    body: vec![Statement::from_pos(
                        StatementKind::Return {
                            return_value: Expression::from_pos(
                                ExpressionKind::Binary {
                                    lhs: Box::new(Expression::from_pos(
                                        ExpressionKind::Binary {
                                            lhs: Box::new(Expression::from_pos(
                                                ExpressionKind::Identifier(String::from("a")),
                                                (11, 10)
                                            )),
                                            operation: Operation::Multiply,
                                            rhs: Box::new(Expression::from_pos(
                                                ExpressionKind::Identifier(String::from("b")),
                                                (15, 10)
                                            )),
                                        },
                                        (11, 10)
                                    )),
                                    operation: Operation::Add,
                                    rhs: Box::new(Expression::from_pos(
                                        ExpressionKind::Identifier(String::from("a")),
                                        (19, 10)
                                    )),
                                },
                                (19, 10)
                            ),
                        },
                        (4, 10)
                    )]
                }),
                (0, 9)
            ),
            Statement::from_pos(
                StatementKind::Function(Function {
                    prototype: Prototype {
                        name: String::from("main"),
                        arguments: vec![
                            (String::from("a"), Type::Int,),
                            (String::from("b"), Type::Int,),
                        ],
                        return_type: Type::Int,
                    },
                    body: vec![Statement::from_pos(
                        StatementKind::Return {
                            return_value: Expression::from_pos(
                                ExpressionKind::Call {
                                    callee: String::from("max"),
                                    arguments: vec![
                                        Expression::from_pos(
                                            ExpressionKind::Identifier(String::from("1"),),
                                            (15, 14)
                                        ),
                                        Expression::from_pos(
                                            ExpressionKind::Identifier(String::from("2"),),
                                            (18, 14)
                                        ),
                                        Expression::from_pos(
                                            ExpressionKind::Identifier(String::from("3"),),
                                            (21, 14)
                                        ),
                                    ],
                                },
                                (11, 14)
                            ),
                        },
                        (4, 14)
                    )],
                }),
                (0, 13)
            ),
        ]
    );
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
