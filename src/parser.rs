use core::panic;

use crate::{
    ast::{
        Assignment, Call, Declaration, Expression, ForStatement, Function, IfStatement, Operation,
        Prototype, Statement,
    },
    compile_error::CompileError,
    lexer::Token,
    types::Type,
    utils::Mutable,
};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub pos: usize,
}

impl Parser {
    pub fn next(&mut self) -> Option<Token> {
        self.pos += 1;
        return self.tokens.get(self.pos - 1).cloned();
    }

    pub fn peek(&mut self) -> Option<&Token> {
        return self.tokens.get(self.pos);
    }

    pub fn peek_forward(&mut self, index: usize) -> Option<&Token> {
        return self.tokens.get(self.pos + index);
    }

    pub fn expect(&mut self, token: Token) -> Result<(), CompileError> {
        if let Some(t) = self.next() {
            if t == token {
                return Ok(());
            } else {
                return Err(CompileError::SyntaxError(format!(
                    "Expected \"{:?}\", found \"{:?}\"",
                    token, t
                )));
            }
        }
        Err(CompileError::SyntaxError(format!(
            "Expected \"{:?}\", found EOF",
            token
        )))
    }

    pub fn check(&mut self, token: Token) -> bool {
        if let Some(t) = self.peek() {
            if *t == token {
                return true;
            }
        }
        false
    }

    pub fn check_forward(&mut self, token: Token, index: usize) -> bool {
        if let Some(t) = self.peek_forward(index) {
            if *t == token {
                return true;
            }
        }
        false
    }

    pub fn optional(&mut self, token: Token) {
        if let Some(t) = self.peek() {
            if *t == token {
                self.next();
            }
        }
    }

    pub fn consume_bang(&mut self) {
        while self.check(Token::Bang) {
            self.next();
        }
    }

    // FIX: merge this into the parse_value attribute
    pub fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        return match self.peek() {
            Some(&Token::Symbol(_)) => {
                if self.check_forward(Token::Eq, 1) {
                    if let Some(Token::Symbol(lhs)) = self.next() {
                        self.expect(Token::Eq)?;
                        let rhs = self.parse_expression()?;
                        Ok(Expression::Assignment(Assignment {
                            lhs,
                            rhs: Box::new(rhs),
                        }))
                    } else {
                        panic!("Invalid state")
                    }
                } else {
                    Ok(self.parse_equality()?)
                }
            }
            Some(&Token::String(_)) => {
                if let Some(Token::String(str)) = self.next() {
                    Ok(Expression::LiteralValue(str))
                } else {
                    panic!("Should not be here.")
                }
            }
            tkn => Err(CompileError::SyntaxError(format!(
                "Expected expression, found {:?}",
                tkn
            ))),
        };
    }

    // FIX: add proper error handling, rather than panicing
    // Only parses identifiers, litervals or function calls.
    pub fn parse_value(&mut self) -> Result<Expression, CompileError> {
        if let Some(token) = self.next() {
            return match token {
                Token::Symbol(sym) => {
                    if let Some(Token::OpenPar) = self.peek() {
                        self.next();
                        self.parse_call(sym)
                    } else {
                        Ok(Expression::Identifier(sym))
                    }
                }
                Token::String(str) => Ok(Expression::LiteralValue(str)),
                tkn => Err(CompileError::SyntaxError(format!(
                    "Unkown token {:?} found in place of value",
                    tkn
                ))),
            };
        }
        Err(CompileError::SyntaxError(String::from(
            "Found eof instead of value",
        )))
    }

    pub fn parse_equality(&mut self) -> Result<Expression, CompileError> {
        let mut expr = self.parse_term()?;

        while let Some(Token::EqEq | Token::EqEqEq | Token::EqEqEqEq | Token::Lt | Token::Gt) =
            self.peek()
        {
            let operation = match self.next().unwrap() {
                Token::EqEq => Operation::Equal,
                Token::EqEqEq => Operation::StrictEqual,
                Token::EqEqEqEq => Operation::VeryStrictEqual,
                Token::Lt => Operation::Less,
                Token::Gt => Operation::Greater,
                _ => panic!("Cannot enter"),
            };
            let rhs = self.parse_term();
            expr = Expression::Binary {
                lhs: Box::new(expr),
                operation,
                rhs: Box::new(rhs?),
            };
        }

        Ok(expr)
    }

    pub fn parse_term(&mut self) -> Result<Expression, CompileError> {
        let mut expr = self.parse_factor()?;

        while let Some(Token::Plus | Token::Dash) = self.peek() {
            let operation = match self.next().unwrap() {
                Token::Plus => Operation::Add,
                Token::Dash => Operation::Subtract,
                _ => panic!("Invalid operation"),
            };
            let rhs = self.parse_factor();

            expr = Expression::Binary {
                lhs: Box::new(expr),
                rhs: Box::new(rhs?),
                operation,
            };
        }

        Ok(expr)
    }

    pub fn parse_factor(&mut self) -> Result<Expression, CompileError> {
        let mut expr = self.parse_value()?;

        while let Some(Token::Star | Token::Slash) = self.peek() {
            let operation = match self.next().unwrap() {
                Token::Star => Operation::Multiply,
                Token::Slash => Operation::Divide,
                _ => panic!("Invalid operation"),
            };
            let rhs = self.parse_value();

            expr = Expression::Binary {
                lhs: Box::new(expr),
                rhs: Box::new(rhs?),
                operation,
            };
        }

        Ok(expr)
    }

    fn parse_call(&mut self, callee: String) -> Result<Expression, CompileError> {
        let mut args = Vec::new();
        while let Some(token) = self.peek() {
            if *token == Token::ClosePar {
                self.next();
                return Ok(Expression::Call(Call {
                    callee,
                    arguments: args,
                }));
            }

            args.push(self.parse_expression()?);

            if Some(&Token::ClosePar) != self.peek() {
                if let Some(Token::Comma) = self.peek() {
                    self.next();
                } else {
                    panic!("Invalid syntax: no comma, found instead {:?}", self.peek())
                }
            }
        }
        Ok(Expression::Unkown)
    }

    pub fn parse_if(&mut self) -> Result<Statement, CompileError> {
        self.expect(Token::If)?;
        self.expect(Token::OpenPar)?;
        let bool_exp = self.parse_expression();
        self.expect(Token::ClosePar)?;
        let body = if let Some(Token::OpenCurB) = self.peek() {
            self.parse_body()?
        } else {
            // TODO: Should this be a parse_statement?
            // Is `function main() => function hi() => {}`
            // valid?
            vec![Statement::Expression(self.parse_expression()?)]
        };

        Ok(Statement::If(Box::new(IfStatement {
            boolean_op: bool_exp?,
            then_statements: body,
            else_statements: None,
        })))
    }

    pub fn parse_for(&mut self) -> Result<Statement, CompileError> {
        self.expect(Token::For)?;
        self.expect(Token::OpenPar)?;
        let initialiser = self.parse_declaration();

        self.expect(Token::Semicolon)?;
        let condition = self.parse_expression();

        self.expect(Token::Semicolon)?;
        let accumalator = self.parse_expression();
        self.expect(Token::ClosePar)?;

        let body = if let Some(Token::OpenCurB) = self.peek() {
            self.parse_body()?
        } else {
            // TODO: Should this be a parse_statement?
            // Is `function main() => function hi() => {}`
            // valid?
            vec![Statement::Expression(self.parse_expression()?)]
        };

        Ok(Statement::For(Box::new(ForStatement {
            initialiser: initialiser?,
            condition: condition?,
            accumalator: accumalator?,
            body: Some(body),
        })))
    }

    pub fn parse_function(&mut self) -> Result<Statement, CompileError> {
        let prototype = self.parse_prototype()?;
        self.expect(Token::Arrow)?;
        let body = if let Some(Token::OpenCurB) = self.peek() {
            self.parse_body()?
        } else {
            // TODO: Should this be a parse_statement?
            // Is `function main() => function hi() => {}`
            // valid?
            vec![Statement::Expression(self.parse_expression()?)]
        };
        Ok(Statement::Function(Box::new(Function { prototype, body })))
    }

    pub fn parse_prototype(&mut self) -> Result<Prototype, CompileError> {
        self.expect(Token::Function)?;

        if let Some(Token::Symbol(name)) = self.next() {
            self.expect(Token::OpenPar)?;
            let mut arguments = Vec::new();
            while let Some(t) = self.next() {
                if let Token::ClosePar = t {
                    if self.peek() == Some(&Token::Colon) {
                        self.next();
                        if let Some(Token::Symbol(t)) = self.next() {
                            return Ok(Prototype {
                                name,
                                arguments,
                                return_type: Type::parse(t),
                            });
                        } else {
                            panic!("Unexpected symbol!!")
                        }
                    } else {
                        return Ok(Prototype {
                            name,
                            arguments,
                            return_type: Type::Void,
                        });
                    }
                }

                if let Token::Symbol(arg) = t {
                    self.expect(Token::Colon)?;
                    if let Some(Token::Symbol(t)) = self.next() {
                        arguments.push((arg, Type::parse(t)));
                        if let Some(Token::ClosePar) = self.peek() {
                            continue;
                        } else {
                            self.expect(Token::Comma)?;
                        }
                    } else {
                        panic!("No type")
                    }
                } else {
                    panic!("Unexpected token {:?}", t)
                }
            }
        }

        panic!("Awef")
    }

    pub fn parse_body(&mut self) -> Result<Vec<Statement>, CompileError> {
        let mut statements = Vec::new();
        self.expect(Token::OpenCurB)?;
        while !self.check(Token::CloseCurB) {
            match self.peek() {
                Some(Token::Const | Token::Var) => statements.push(self.parse_declaration()?),
                Some(Token::Return) => statements.push(self.parse_return()?),
                Some(Token::If) => statements.push(self.parse_if()?),
                Some(Token::For) => statements.push(self.parse_for()?),
                Some(Token::Function) => statements.push(self.parse_function()?),
                _ => {
                    statements.push(Statement::Expression(self.parse_expression()?));
                    self.consume_bang();
                }
            }
        }
        self.expect(Token::CloseCurB)?;
        Ok(statements)
    }

    pub fn parse_declaration(&mut self) -> Result<Statement, CompileError> {
        let mut flags = Mutable::NONE;
        if let Some(first_op) = self.next() {
            if let Some(second_op) = self.next() {
                const ALLOWED_TOKENS: [Token; 2] = [Token::Var, Token::Const];
                if !ALLOWED_TOKENS.contains(&first_op) || !ALLOWED_TOKENS.contains(&second_op) {
                    return Err(CompileError::SyntaxError(String::from("Declaration is missing var/const")));
                }
                if first_op == Token::Var {
                    flags |= Mutable::Reassignable;
                }
                if second_op == Token::Var {
                    flags |= Mutable::Modifiable;
                }
            }
        }

        if let Some(Token::Symbol(lhs)) = self.next() {
            if let Some(Token::Eq) = self.next() {
                let rhs = self.parse_expression();
                self.consume_bang();
                Ok(Statement::Declaration(Box::new(Declaration {
                    mutable: flags,
                    lhs,
                    rhs: rhs?,
                })))
            } else {
                Err(CompileError::SyntaxError(String::from("Expected '=' in the declaration.")))
            }
        } else {
            Err(CompileError::SyntaxError(String::from("Missing identifier in declaration")))
        }
    }

    fn parse_return(&mut self) -> Result<Statement, CompileError> {
        self.expect(Token::Return)?;
        let return_value = self.parse_expression()?;
        // Should probably be in statement
        self.optional(Token::Bang);
        Ok(Statement::Return {
            return_value: Box::new(return_value),
        })
    }
}
