use std::iter::Peekable;

use crate::{
    ast::{
        AssignmentExpression, CallExpression, DeclarationStatement, Expression, FunctionStatement,
        Prototype, Statement,
    },
    lexer::Token,
    utils::Mutable,
};

pub fn expect<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>, token: Token) {
    if let Some(t) = tokens.next() {
        if t == token {
            return;
        }
    }
    panic!("Unexpected token {:?}", token)
}

pub fn check<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>, token: Token) -> bool {
    if let Some(t) = tokens.peek() {
        if *t == token {
            return true;
        }
    }
    return false;
}

pub fn consume_bang<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) {
    while check(tokens, Token::Bang) {
        tokens.next();
    }
}

pub fn parse_expression<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Expression {
    while let Some(token) = tokens.next() {
        return match token {
            Token::Symbol(sym) => {
                if let Some(Token::OpenPar) = tokens.peek() {
                    tokens.next();
                    parse_call(tokens, sym)
                } else {
                    if check(tokens, Token::Eq) {
                        expect(tokens, Token::Eq);
                        if let Some(Token::Symbol(rhs)) = tokens.next() {
                            Expression::Assignment(AssignmentExpression {
                                lhs: sym,
                                rhs
                            })
                        } else {
                            panic!("Invalid syntax: in assignment")
                        }
                    } else {
                        Expression::Identifier(sym)
                    }
                }
            }
            Token::String(str) => Expression::LiteralValue(str),
            _ => Expression::Unkown,
        };
    }
    panic!("Invalid expression")
}

fn parse_call<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>, callee: String) -> Expression {
    let mut args = Vec::new();
    while let Some(token) = tokens.peek() {
        if *token == Token::ClosePar {
            tokens.next();
            return Expression::CallExpression(CallExpression {
                callee,
                arguments: args,
            });
        }

        args.push(parse_expression(tokens));

        if Some(&Token::ClosePar) != tokens.peek() {
            if let Some(Token::Comma) = tokens.peek() {
                tokens.next();
            } else {
                panic!(
                    "Invalid syntax: no comma, found instead {:?}",
                    tokens.peek()
                )
            }
        }
    }
    Expression::Unkown
}

pub fn parse_function<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Statement {
    let prototype = parse_prototype(tokens);
    let mut body = Vec::new();
    expect(tokens, Token::Arrow);
    if let Some(Token::OpenCurB) = tokens.peek() {
        for statement in parse_body(tokens) {
            body.push(statement);
        }
    } else {
        // TODO: Should this be a parse_statement?
        // Is `function main() => function hi() => {}`
        // valid?
        body.push(Statement::Expression(parse_expression(tokens)));
    }
    return Statement::Function(Box::new(FunctionStatement { prototype, body }));
}

pub fn parse_prototype<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Prototype {
    expect(tokens, Token::Function);

    if let Some(Token::Symbol(name)) = tokens.next() {
        expect(tokens, Token::OpenPar);
        let mut arguments = Vec::new();
        while let Some(t) = tokens.next() {
            if let Token::ClosePar = t {
                return Prototype { name, arguments };
            } else if let Token::Symbol(arg) = t {
                arguments.push(arg);
                if let Some(Token::ClosePar) = tokens.peek() {
                    continue;
                } else {
                    expect(tokens, Token::Comma);
                }
            } else {
                panic!("Unexpected token")
            }
        }
    }

    panic!("Awef")
}

pub fn parse_body<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Vec<Statement> {
    let mut statements = Vec::new();
    expect(tokens, Token::OpenCurB);
    while !check(tokens, Token::CloseCurB) {
        if check(tokens, Token::Const) || check(tokens, Token::Var) {
            statements.push(parse_declaration(tokens));
        } else if check(tokens, Token::Function) {
            statements.push(parse_function(tokens));
        } else {
            statements.push(Statement::Expression(parse_expression(tokens)));
            consume_bang(tokens);
        }
    }
    expect(tokens, Token::CloseCurB);
    return statements;
}

pub fn parse_declaration<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Statement {
    let mut flags = Mutable::NONE;
    if let Some(first_op) = tokens.next() {
        if let Some(second_op) = tokens.next() {
            const ALLOWED_TOKENS: [Token; 2] = [Token::Var, Token::Const];
            if !ALLOWED_TOKENS.contains(&first_op) || !ALLOWED_TOKENS.contains(&second_op) {
                panic!("invalid declaration syntax.")
            }
            if first_op == Token::Var {
                flags |= Mutable::Reassignable;
            }
            if second_op == Token::Var {
                flags |= Mutable::Modifiable;
            }
        }
    }

    if let Some(Token::Symbol(lhs)) = tokens.next() {
        if let Some(Token::Eq) = tokens.next() {
            let rhs = parse_expression(tokens);
            consume_bang(tokens);
            return Statement::Declaration(Box::new(DeclarationStatement {
                mutable: flags,
                lhs,
                rhs,
            }));
        } else {
            panic!("Expected '=' in the declaration.")
        }
    } else {
        panic!("Missing identifier for declaration.")
    }
}
