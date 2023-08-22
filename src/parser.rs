use std::iter::Peekable;

use crate::{
    ast::{
        AssignmentExpression, BinaryExpression, CallExpression, DeclarationStatement, Expression,
        FunctionStatement, Operation, Prototype, ReturnStatement, Statement,
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

pub fn optional<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>, token: Token) {
    if let Some(t) = tokens.peek() {
        if *t == token {
            tokens.next();
        }
    }
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
                            Expression::Assignment(AssignmentExpression { lhs: sym, rhs })
                        } else {
                            panic!("Invalid syntax: in assignment")
                        }
                    } else {
                        if check(tokens, Token::Plus)
                            || check(tokens, Token::Dash)
                            || check(tokens, Token::Star)
                            || check(tokens, Token::Slash)
                        {
                            parse_binary(sym, tokens)
                        } else {
                            Expression::Identifier(sym)
                        }
                    }
                }
            }
            Token::String(str) => Expression::LiteralValue(str),
            _ => Expression::Unkown,
        };
    }
    panic!("Invalid expression")
}

pub fn parse_factor<T: Iterator<Item = Token>>(
    lhs: Expression,
    tokens: &mut Peekable<T>,
) -> Expression {
    let mut expr = lhs;

    while let Some(Token::Star | Token::Slash) = tokens.peek() {
        let operation = match tokens.next().unwrap() {
            Token::Star => Operation::Multiply,
            Token::Slash => Operation::Divide,
            _ => panic!("Invalid syntax"),
        };
        // Should parse an expression (everything except binary), but oh well for now
        let rhs = if let Token::Symbol(rhs) = tokens.next().unwrap() {
            Expression::Identifier(rhs)
        } else {
            panic!("No right hand side")
        };

        expr = Expression::BinaryExpression(BinaryExpression {
            lhs: Box::new(expr),
            rhs: Box::new(rhs),
            operation,
        });
    }

    return expr;
}

pub fn parse_term<T: Iterator<Item = Token>>(
    lhs: Expression,
    tokens: &mut Peekable<T>,
) -> Expression {
    let mut expr = parse_factor(lhs, tokens);

    while let Some(Token::Plus | Token::Dash) = tokens.peek() {
        let operation = match tokens.next().unwrap() {
            Token::Plus => Operation::Add,
            Token::Dash => Operation::Subtract,
            _ => panic!("Invalid syntax"),
        };
        // Should parse an expression (everything except binary), but oh well for now
        let rhs = if let Token::Symbol(rhs) = tokens.next().unwrap() {
            Expression::Identifier(rhs)
        } else {
            panic!("No right hand side")
        };
        let rhs = parse_factor(rhs, tokens);

        expr = Expression::BinaryExpression(BinaryExpression {
            lhs: Box::new(expr),
            rhs: Box::new(rhs),
            operation,
        });
    }

    return expr;
}

fn parse_binary<T: Iterator<Item = Token>>(lhs: String, tokens: &mut Peekable<T>) -> Expression {
    // TODO: parsing it in like this is pretty ugly, should maybe make a Parser struct.
    parse_term(Expression::Identifier(lhs), tokens)
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
            }

            if let Token::Symbol(arg) = t {
                expect(tokens, Token::Colon);
                if let Some(Token::Symbol(t)) = tokens.next() {
                    arguments.push((arg, t));
                    if let Some(Token::ClosePar) = tokens.peek() {
                        continue;
                    } else {
                        expect(tokens, Token::Comma);
                    }
                } else {
                    panic!("No type")
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
        } else if check(tokens, Token::Return) {
            statements.push(parse_return(tokens));
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

fn parse_return<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Statement {
    expect(tokens, Token::Return);
    let return_value = parse_expression(tokens);
    // Should probably be in statement
    optional(tokens, Token::Bang);
    Statement::Return(Box::new(ReturnStatement { return_value }))
}
