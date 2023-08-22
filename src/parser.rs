use crate::{
    ast::{
        AssignmentExpression, BinaryExpression, CallExpression, DeclarationStatement, Expression,
        FunctionStatement, Operation, Prototype, ReturnStatement, Statement,
    },
    lexer::Token, utils::Mutable,
};

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn init(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    pub fn next(&mut self) -> Option<&Token> {
        self.index += 1;
        return self.tokens.get(self.index);
    }

    pub fn expect(&mut self, token: Token) {
        if let Some(t) = self.next() {
            if *t == token {
                return;
            }
        }
        panic!("Unexpected token {:?}", token)
    }

    pub fn peek(&mut self) -> Option<&Token> {
        return self.tokens.get(self.index + 1);
    }

    pub fn current(&mut self) -> &Token {
        return &self.tokens[self.index];
    }

    // Expects that the value is greater than 1.
    pub fn previous(&mut self) -> &Token {
        return &self.tokens[self.index - 1];
    }

    pub fn check(&mut self, token: Token) -> bool {
        if let Some(t) = self.peek() {
            if *t == token {
                return true;
            }
        }
        return false;
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

    pub fn parse_expression(&mut self) -> Expression {
        while let Some(&token) = self.next() {
            return match token {
                Token::Symbol(sym) => {
                    if let Some(Token::OpenPar) = self.peek() {
                        self.next();
                        self.parse_call(sym)
                    } else {
                        if self.check(Token::Eq) {
                            self.expect(Token::Eq);
                            if let Some(Token::Symbol(rhs)) = self.next() {
                                Expression::Assignment(AssignmentExpression { lhs: sym, rhs: rhs.to_string() })
                            } else {
                                panic!("Invalid syntax: in assignment")
                            }
                        } else {
                            if self.check(Token::Plus)
                                || self.check(Token::Dash)
                                || self.check(Token::Star)
                                || self.check(Token::Slash)
                            {
                                self.parse_binary()
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

    pub fn parse_factor(&mut self) -> Expression {
        let lhs = if let Token::Symbol(lhs) = self.next().unwrap() {
            Expression::Identifier(lhs.to_string())
        } else {
            panic!("No right hand side")
        };

        while let Some(Token::Star | Token::Slash) = self.peek() {
            let operation = match self.next().unwrap() {
                Token::Star => Operation::Multiply,
                Token::Slash => Operation::Divide,
                _ => panic!("Invalid syntax"),
            };
            // Should parse an expression (everything except binary), but oh well for now
            let rhs = if let Token::Symbol(rhs) = self.next().unwrap() {
                Expression::Identifier(rhs.to_string())
            } else {
                panic!("No right hand side")
            };

            return Expression::BinaryExpression(BinaryExpression {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                operation,
            });
        }

        return lhs;
    }

    pub fn parse_term(&mut self) -> Expression {
        let lhs = self.parse_factor();

        while let Some(Token::Plus | Token::Dash) = self.peek() {
            let operation = match self.next().unwrap() {
                Token::Plus => Operation::Add,
                Token::Dash => Operation::Subtract,
                _ => panic!("Invalid syntax"),
            };
            // Should parse an expression (everything except binary), but oh well for now
            let rhs = if let Token::Symbol(rhs) = self.next().unwrap() {
                Expression::Identifier(rhs.to_string())
            } else {
                panic!("No right hand side")
            };

            return Expression::BinaryExpression(BinaryExpression {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                operation,
            });
        }

        return lhs;
    }

    fn parse_binary(&mut self) -> Expression {
        self.parse_term()
    }

    fn parse_call(
        &mut self,
        callee: String,
    ) -> Expression {
        let mut args = Vec::new();
        while let Some(token) = self.peek() {
            if *token == Token::ClosePar {
                self.next();
                return Expression::CallExpression(CallExpression {
                    callee,
                    arguments: args,
                });
            }

            args.push(self.parse_expression());

            if Some(&Token::ClosePar) != self.peek() {
                if let Some(Token::Comma) = self.peek() {
                    self.next();
                } else {
                    panic!(
                        "Invalid syntax: no comma, found instead {:?}",
                        self.peek()
                    )
                }
            }
        }
        Expression::Unkown
    }

    pub fn parse_function(&mut self) -> Statement {
        let prototype = self.parse_prototype();
        let mut body = Vec::new();
        self.expect(Token::Arrow);
        if let Some(Token::OpenCurB) = self.peek() {
            for statement in self.parse_body() {
                body.push(statement);
            }
        } else {
            // TODO: Should this be a parse_statement?
            // Is `function main() => function hi() => {}`
            // valid?
            body.push(Statement::Expression(self.parse_expression()));
        }
        return Statement::Function(Box::new(FunctionStatement { prototype, body }));
    }

    pub fn parse_prototype(&mut self) -> Prototype {
        self.expect(Token::Function);

        if let Some(Token::Symbol(name)) = self.next() {
            self.expect(Token::OpenPar);
            let mut arguments = Vec::new();
            while let Some(t) = self.next() {
                if let Token::ClosePar = t {
                    return Prototype { name: name.to_string(), arguments };
                }

                if let Token::Symbol(arg) = t {
                    self.expect(Token::Colon);
                    if let Some(Token::Symbol(t)) = self.next() {
                        arguments.push((arg.to_string(), t.to_string()));
                        if let Some(Token::ClosePar) = self.peek() {
                            continue;
                        } else {
                            self.expect(Token::Comma);
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

    pub fn parse_body(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        self.expect(Token::OpenCurB);
        while !self.check(Token::CloseCurB) {
            if self.check(Token::Const) || self.check(Token::Var) {
                statements.push(self.parse_declaration());
            } else if self.check(Token::Return) {
                statements.push(self.parse_return());
            } else if self.check(Token::Function) {
                statements.push(self.parse_function());
            } else {
                statements.push(Statement::Expression(self.parse_expression()));
                self.consume_bang();
            }
        }
        self.expect(Token::CloseCurB);
        return statements;
    }

    pub fn parse_declaration(&mut self) -> Statement {
        let mut flags = Mutable::NONE;
        if let Some(first_op) = self.next() {
            if let Some(second_op) = self.next() {
                const ALLOWED_TOKENS: [Token; 2] = [Token::Var, Token::Const];
                if !ALLOWED_TOKENS.contains(&first_op) || !ALLOWED_TOKENS.contains(&second_op) {
                    panic!("invalid declaration syntax.")
                }
                if *first_op == Token::Var {
                    flags |= Mutable::Reassignable;
                }
                if *second_op == Token::Var {
                    flags |= Mutable::Modifiable;
                }
            }
        }

        if let Some(Token::Symbol(lhs)) = self.next() {
            if let Some(Token::Eq) = self.next() {
                let rhs = self.parse_expression();
                self.consume_bang();
                return Statement::Declaration(Box::new(DeclarationStatement {
                    mutable: flags,
                    lhs: lhs.to_string(),
                    rhs,
                }));
            } else {
                panic!("Expected '=' in the declaration.")
            }
        } else {
            panic!("Missing identifier for declaration.")
        }
    }

    fn parse_return(&mut self) -> Statement {
        self.expect(Token::Return);
        let return_value = self.parse_expression();
        // Should probably be in statement
        self.optional(Token::Bang);
        Statement::Return(Box::new(ReturnStatement { return_value }))
    }
}
