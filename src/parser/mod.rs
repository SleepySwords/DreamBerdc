use core::panic;

use crate::{
    ast::{
        Assignment, Call, Declaration, Expression, ForStatement, Function, IfStatement, Operation,
        Prototype, Statement,
    },
    compile_error::CompileError,
    lexer::{Token, TokenKind},
    types::Type,
    utils::Mutable,
};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub pos: usize,
}

impl Parser {
    pub fn next(&mut self) -> Option<TokenKind> {
        self.pos += 1;
        return self.tokens.get(self.pos - 1).map(|f| f.kind.clone());
    }

    pub fn peek(&self) -> Option<&TokenKind> {
        return self.tokens.get(self.pos).map(|x| &x.kind);
    }

    pub fn peek_forward(&mut self, index: usize) -> Option<&TokenKind> {
        return self.tokens.get(self.pos + index).map(|f| &f.kind);
    }

    pub fn current_pos(&self) -> (usize, usize) {
        let token = self.tokens.get(self.pos).unwrap();
        (token.col, token.lnum)
    }

    pub fn previous_pos(&self) -> (usize, usize) {
        let token = self.tokens.get(self.pos - 1).unwrap();
        (token.col, token.lnum)
    }

    pub fn expect(&mut self, token: TokenKind) -> Result<(), CompileError> {
        if let Some(t) = self.next() {
            if t == token {
                return Ok(());
            } else {
                return Err(CompileError::SyntaxError(
                    self.previous_pos(),
                    format!("Expected \"{:?}\", found \"{:?}\"", token, t),
                ));
            }
        }
        Err(CompileError::SyntaxError(
            self.current_pos(),
            format!("Expected \"{:?}\", found EOF", token),
        ))
    }

    pub fn check(&mut self, token: TokenKind) -> bool {
        if let Some(t) = self.peek() {
            if *t == token {
                return true;
            }
        }
        false
    }

    pub fn check_forward(&mut self, token: TokenKind, index: usize) -> bool {
        if let Some(t) = self.peek_forward(index) {
            if *t == token {
                return true;
            }
        }
        false
    }

    pub fn optional(&mut self, token: TokenKind) {
        if let Some(t) = self.peek() {
            if *t == token {
                self.next();
            }
        }
    }

    pub fn consume_bang(&mut self) {
        while self.check(TokenKind::Bang) {
            self.next();
        }
    }

    // FIX: merge this into the parse_value attribute
    pub fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        let pos = self.current_pos();
        return match self.peek() {
            Some(&TokenKind::Symbol(_)) => {
                if self.check_forward(TokenKind::Eq, 1) {
                    if let Some(TokenKind::Symbol(lhs)) = self.next() {
                        self.expect(TokenKind::Eq)?;
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
            Some(&TokenKind::String(_)) => {
                if let Some(TokenKind::String(str)) = self.next() {
                    Ok(Expression::LiteralValue(str))
                } else {
                    panic!("Should not be here.")
                }
            }
            Some(&TokenKind::OpenSqB) => {
                return self.parse_array();
            }
            tkn => Err(CompileError::SyntaxError(
                pos,
                format!(
                    "Expected expression, found {:?}",
                    tkn.map(|f| format!("{:?}", f))
                        .unwrap_or("none".to_string())
                ),
            )),
        };
    }

    // FIX: add proper error handling, rather than panicing
    // Only parses identifiers, litervals or function calls.
    pub fn parse_value(&mut self) -> Result<Expression, CompileError> {
        if let Some(token) = self.next() {
            return match token {
                TokenKind::Symbol(sym) => {
                    if let Some(TokenKind::OpenPar) = self.peek() {
                        self.next();
                        self.parse_call(sym)
                    } else {
                        Ok(Expression::Identifier(sym))
                    }
                }
                TokenKind::String(str) => Ok(Expression::LiteralValue(str)),
                tkn => Err(CompileError::SyntaxError(
                    self.previous_pos(),
                    format!("Expected value, found {:?}", tkn),
                )),
            };
        }
        panic!("Should not happen")
    }

    pub fn parse_equality(&mut self) -> Result<Expression, CompileError> {
        let mut expr = self.parse_term()?;

        while let Some(
            TokenKind::EqEq
            | TokenKind::EqEqEq
            | TokenKind::EqEqEqEq
            | TokenKind::Lt
            | TokenKind::Gt,
        ) = self.peek()
        {
            let operation = match self.next().unwrap() {
                TokenKind::EqEq => Operation::Equal,
                TokenKind::EqEqEq => Operation::StrictEqual,
                TokenKind::EqEqEqEq => Operation::VeryStrictEqual,
                TokenKind::Lt => Operation::Less,
                TokenKind::Gt => Operation::Greater,
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

        while let Some(TokenKind::Plus | TokenKind::Dash) = self.peek() {
            let operation = match self.next().unwrap() {
                TokenKind::Plus => Operation::Add,
                TokenKind::Dash => Operation::Subtract,
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

        while let Some(TokenKind::Star | TokenKind::Slash) = self.peek() {
            let operation = match self.next().unwrap() {
                TokenKind::Star => Operation::Multiply,
                TokenKind::Slash => Operation::Divide,
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
            if *token == TokenKind::ClosePar {
                self.next();
                return Ok(Expression::Call(Call {
                    callee,
                    arguments: args,
                }));
            }

            args.push(self.parse_expression()?);

            if Some(&TokenKind::ClosePar) != self.peek() {
                if let Some(TokenKind::Comma) = self.peek() {
                    self.next();
                } else {
                    return Err(CompileError::SyntaxError(
                        self.current_pos(),
                        format!(
                            "Expected comma or closing bracked, found {:?}",
                            self.peek()
                                .map(|f| format!("{:?}", f))
                                .unwrap_or("none".to_string())
                        ),
                    ));
                }
            }
        }
        Ok(Expression::Unkown)
    }

    pub fn parse_if(&mut self) -> Result<Statement, CompileError> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::OpenPar)?;
        let bool_exp = self.parse_expression();
        self.expect(TokenKind::ClosePar)?;
        let (body, else_body) = if let Some(TokenKind::OpenCurB) = self.peek() {
            let body = self.parse_body()?;
            if self.check(TokenKind::Else) {
                self.expect(TokenKind::Else)?;
                let else_body = self.parse_body()?;
                (body, Some(else_body))
            } else {
                (body, None)
            }
        } else {
            (vec![Statement::Expression(self.parse_expression()?)], None)
        };

        Ok(Statement::If(Box::new(IfStatement {
            boolean_op: bool_exp?,
            then_statements: body,
            else_statements: else_body,
        })))
    }

    pub fn parse_for(&mut self) -> Result<Statement, CompileError> {
        self.expect(TokenKind::For)?;
        self.expect(TokenKind::OpenPar)?;
        let initialiser = self.parse_declaration();

        self.expect(TokenKind::Semicolon)?;
        let condition = self.parse_expression();

        self.expect(TokenKind::Semicolon)?;
        let accumalator = self.parse_expression();
        self.expect(TokenKind::ClosePar)?;

        let body = if let Some(TokenKind::OpenCurB) = self.peek() {
            self.parse_body()?
        } else {
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
        self.expect(TokenKind::Arrow)?;
        let body = if let Some(TokenKind::OpenCurB) = self.peek() {
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
        self.expect(TokenKind::Function)?;

        if let Some(TokenKind::Symbol(name)) = self.next() {
            self.expect(TokenKind::OpenPar)?;
            let mut arguments = Vec::new();
            while let Some(t) = self.next() {
                if let TokenKind::ClosePar = t {
                    if self.peek() == Some(&TokenKind::Colon) {
                        self.next();
                        if let Some(TokenKind::Symbol(t)) = self.next() {
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

                if let TokenKind::Symbol(arg) = t {
                    self.expect(TokenKind::Colon)?;
                    if let Some(TokenKind::Symbol(t)) = self.next() {
                        arguments.push((arg, Type::parse(t)));
                        if let Some(TokenKind::ClosePar) = self.peek() {
                            continue;
                        } else {
                            self.expect(TokenKind::Comma)?;
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
        self.expect(TokenKind::OpenCurB)?;
        while !self.check(TokenKind::CloseCurB) {
            match self.peek() {
                Some(TokenKind::Const | TokenKind::Var) => {
                    statements.push(self.parse_declaration()?)
                }
                Some(TokenKind::Return) => statements.push(self.parse_return()?),
                Some(TokenKind::If) => statements.push(self.parse_if()?),
                Some(TokenKind::For) => statements.push(self.parse_for()?),
                Some(TokenKind::Function) => statements.push(self.parse_function()?),
                _ => {
                    statements.push(Statement::Expression(self.parse_expression()?));
                    self.consume_bang();
                }
            }
        }
        self.expect(TokenKind::CloseCurB)?;
        Ok(statements)
    }

    pub fn parse_declaration(&mut self) -> Result<Statement, CompileError> {
        let mut flags = Mutable::NONE;
        if let Some(first_op) = self.next() {
            if let Some(second_op) = self.next() {
                const ALLOWED_TOKENS: [TokenKind; 2] = [TokenKind::Var, TokenKind::Const];
                if !ALLOWED_TOKENS.contains(&first_op) || !ALLOWED_TOKENS.contains(&second_op) {
                    return Err(CompileError::SyntaxError(
                        self.current_pos(),
                        String::from("Declaration is missing var/const"),
                    ));
                }
                if first_op == TokenKind::Var {
                    flags |= Mutable::Reassignable;
                }
                if second_op == TokenKind::Var {
                    flags |= Mutable::Modifiable;
                }
            }
        }

        if let Some(TokenKind::Symbol(lhs)) = self.next() {
            if let Some(TokenKind::Eq) = self.next() {
                let rhs = self.parse_expression();
                self.consume_bang();
                Ok(Statement::Declaration(Box::new(Declaration {
                    mutable: flags,
                    lhs,
                    rhs: rhs?,
                })))
            } else {
                Err(CompileError::SyntaxError(
                    self.current_pos(),
                    String::from("Expected '=' in the declaration."),
                ))
            }
        } else {
            Err(CompileError::SyntaxError(
                self.current_pos(),
                String::from("Missing identifier in declaration"),
            ))
        }
    }

    fn parse_return(&mut self) -> Result<Statement, CompileError> {
        self.expect(TokenKind::Return)?;
        let return_value = self.parse_expression()?;
        // Should probably be in statement
        self.optional(TokenKind::Bang);
        Ok(Statement::Return {
            return_value: Box::new(return_value),
        })
    }

    fn parse_array(&mut self) -> Result<Expression, CompileError> {
        self.expect(TokenKind::OpenSqB)?;
        let mut values = vec![];
        while !self.check(TokenKind::CloseSqB) {
            let expr = self.parse_expression()?;
            values.push(expr);
            // FIXME: Disgusting syntax
            if self.check(TokenKind::Comma) {
                self.expect(TokenKind::Comma)?;
            } else if !self.check(TokenKind::CloseSqB) {
                // FIXME: do this better
                return Err(CompileError::SyntaxError(
                    self.current_pos(),
                    String::from("Unexpected symbol"),
                ));
            }
        }
        self.expect(TokenKind::CloseSqB)?;
        Ok(Expression::Array(values))
    }
}
