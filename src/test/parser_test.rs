use itertools::Itertools;

use crate::{
    ast::{Declaration, Expression, Function, Operation, Prototype, Statement},
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
            // Statement::Function(Function {
            //     prototype: Prototype {
            //         name: "max".to_string(),
            //         arguments: vec![
            //             ("a".to_string(), Type::Int,),
            //             ("b".to_string(), Type::Int,),
            //             ("c".to_string(), Type::Int,),
            //         ],
            //         return_type: Type::Int,
            //     },
            //     body: vec![
            //         Statement::Declaration(Declaration {
            //             mutable: Mutable::ALL,
            //             lhs: "d".to_string(),
            //             rhs: Expression::Identifier("0".to_string(),),
            //         },),
            //         Statement::For(Statement::ForStatement {
            //             initialiser: Declaration(Declaration {
            //                 mutable: Mutable(Reassignable | Modifiable,),
            //                 lhs: "i".to_string(),
            //                 rhs: Identifier("0".to_string(),),
            //             },),
            //             condition: Binary {
            //                 lhs: Identifier("i".to_string(),),
            //                 operation: Less,
            //                 rhs: Identifier("10".to_string(),),
            //             },
            //             accumalator: Identifier("0".to_string(),),
            //             body: Some([Expression(Assignment(Assignment {
            //                 lhs: "d".to_string(),
            //                 rhs: Binary {
            //                     lhs: Identifier("d".to_string(),),
            //                     operation: Add,
            //                     rhs: Identifier("i".to_string(),),
            //                 },
            //             },),),],),
            //         },),
            //         Expression(Call {
            //             callee: "putchar".to_string(),
            //             arguments: [Binary {
            //                 lhs: Identifier("d".to_string(),),
            //                 operation: Add,
            //                 rhs: Identifier("65".to_string(),),
            //             },],
            //         },),
            //         Return {
            //             return_value: Identifier("d".to_string(),),
            //         },
            //     ],
            // },),
            // Function(Function {
            //     prototype: Prototype {
            //         name: "add".to_string(),
            //         arguments: [("a".to_string(), Int,), ("b".to_string(), Int,),],
            //         return_type: Void,
            //     },
            //     body: [Return {
            //         return_value: Binary {
            //             lhs: Binary {
            //                 lhs: Identifier("a".to_string(),),
            //                 operation: Multiply,
            //                 rhs: Identifier("b".to_string(),),
            //             },
            //             operation: Add,
            //             rhs: Identifier("a".to_string(),),
            //         },
            //     },],
            // },),
            // Function(Function {
            //     prototype: Prototype {
            //         name: "main".to_string(),
            //         arguments: [("a".to_string(), Int,), ("b".to_string(), Int,),],
            //         return_type: Int,
            //     },
            //     body: [Return {
            //         return_value: Call {
            //             callee: "max".to_string(),
            //             arguments: [
            //                 Identifier("1".to_string(),),
            //                 Identifier("2".to_string(),),
            //                 Identifier("3".to_string(),),
            //             ],
            //         },
            //     },],
            // },),
        ]
    );
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
