use itertools::Itertools;

use crate::{
    lexer::Lexer,
    parser::Parser,
    TokenKind,
};
use std::{error::Error, fs};

#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_parse_add() -> Result<(), Box<dyn Error>> {
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
        format!("{:#?}", statements),
        "[
    Statement {
        kind: Function(
            Function {
                prototype: Prototype {
                    name: \"add\",
                    arguments: [
                        (
                            \"a\",
                            Int,
                        ),
                        (
                            \"b\",
                            Int,
                        ),
                    ],
                    return_type: Int,
                },
                body: [
                    Statement {
                        kind: Return {
                            return_value: Some(
                                Expression {
                                    kind: Binary {
                                        lhs: Expression {
                                            kind: Identifier(
                                                \"a\",
                                            ),
                                            lnum: 1,
                                            col: 11,
                                        },
                                        operation: Add,
                                        rhs: Expression {
                                            kind: Identifier(
                                                \"b\",
                                            ),
                                            lnum: 1,
                                            col: 15,
                                        },
                                    },
                                    lnum: 1,
                                    col: 15,
                                },
                            ),
                        },
                        lnum: 1,
                        col: 4,
                    },
                ],
            },
        ),
        lnum: 0,
        col: 0,
    },
    Statement {
        kind: Function(
            Function {
                prototype: Prototype {
                    name: \"main\",
                    arguments: [],
                    return_type: Void,
                },
                body: [
                    Statement {
                        kind: Return {
                            return_value: Some(
                                Expression {
                                    kind: Call {
                                        callee: \"add\",
                                        arguments: [
                                            Expression {
                                                kind: Identifier(
                                                    \"1\",
                                                ),
                                                lnum: 5,
                                                col: 15,
                                            },
                                            Expression {
                                                kind: Identifier(
                                                    \"5\",
                                                ),
                                                lnum: 5,
                                                col: 17,
                                            },
                                        ],
                                    },
                                    lnum: 5,
                                    col: 11,
                                },
                            ),
                        },
                        lnum: 5,
                        col: 4,
                    },
                ],
            },
        ),
        lnum: 4,
        col: 0,
    },
]"
    );
    // '<,'>s/\[/vec![/g
    //'<,'>s/\(".\{-}"\)/\1.to_string/g
    Ok(()) // '<,'>s/\(.\{-}\)-(\(.\{-}\), \(.\{-}\)),/Token::new(\1, \3, \2), /g
}
