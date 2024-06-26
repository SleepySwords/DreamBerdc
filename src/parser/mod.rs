// FIXME: create a take if match macro
// To clean all the peek and next.
mod parser_class;
mod parser_ops;

use crate::{
    ast::{
        Declaration, Expression, ExpressionKind, ForStatement, Function, IfStatement, Prototype,
        SourcePosition, Statement, StatementKind,
    },
    compile_error::CompilerError,
    lexer::{Token, TokenKind},
    types::Type,
    utils::Mutable,
};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub pos: usize,
}

impl Parser {
    /// Returns the current token and increments the position.
    pub fn next(&mut self) -> Option<TokenKind> {
        self.pos += 1;
        return self.tokens.get(self.pos - 1).map(|f| f.kind.clone());
    }

    /// Returns the current token without incrementing the position.
    pub fn peek(&self) -> Option<&TokenKind> {
        return self.tokens.get(self.pos).map(|x| &x.kind);
    }

    pub fn peek_forward(&self, index: usize) -> Option<&TokenKind> {
        return self.tokens.get(self.pos + index).map(|f| &f.kind);
    }

    pub fn current_pos(&self) -> SourcePosition {
        let token = self.tokens.get(self.pos).unwrap();
        (token.col, token.lnum)
    }

    pub fn previous_pos(&self) -> SourcePosition {
        let token = self.tokens.get(self.pos - 1).unwrap();
        (token.col, token.lnum)
    }

    // Should probably support pattern matching.
    pub fn expect(&mut self, token: TokenKind) -> Result<(), CompilerError> {
        if let Some(t) = self.next() {
            if t == token {
                return Ok(());
            } else {
                return Err(CompilerError::SyntaxError(
                    self.previous_pos(),
                    format!("Expected \"{:?}\", found \"{:?}\"", token, t),
                ));
            }
        }
        Err(CompilerError::SyntaxError(
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

    #[allow(dead_code)]
    pub fn check_forward(&self, token: TokenKind, index: usize) -> bool {
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

    pub fn parse_expression(&mut self) -> Result<Expression, CompilerError> {
        let expression_pos = self.current_pos();

        parse_sequence!(self,
            (TokenKind::Symbol(lhs), TokenKind::Eq) => {
                let rhs = self.parse_expression()?;
                return Ok(Expression::from_pos(
                    ExpressionKind::Assignment {
                        lhs,
                        rhs: Box::new(rhs),
                    },
                    expression_pos,
                ));
            },
            (TokenKind::Symbol(lhs), operation if operation.is_compound()) => {
                let rhs = self.parse_expression()?;
                let binary = ExpressionKind::Binary {
                    lhs: Box::new(Expression::from_pos(
                        ExpressionKind::Identifier(lhs.clone()),
                        expression_pos,
                    )),
                    operation: operation.operation_compound().unwrap(),
                    rhs: Box::new(rhs),
                };
                return Ok(Expression::from_pos(
                    ExpressionKind::Assignment {
                        lhs,
                        rhs: Box::new(Expression::from_pos(binary, expression_pos)),
                    },
                    expression_pos,
                ))
            },
            (TokenKind::String(str)) => {
                return Ok(Expression::from_pos(
                    ExpressionKind::LiteralValue(str),
                    expression_pos,
                ));
            },
            (TokenKind::Star) => {
                let expression = self.parse_expression()?;
                return Ok(Expression::from_pos(
                    ExpressionKind::Dereference(Box::new(expression)),
                    expression_pos,
                ));
            }
        );

        // NOTE: Methods here consume their tokens by themselves, so they cannot
        // be used as above, because the above consumes the tokens.
        match self.peek() {
            Some(&TokenKind::OpenSqB) => self.parse_array(),
            _ => self.parse_equality(),
        }
    }


    pub fn parse_if(&mut self) -> Result<Statement, CompilerError> {
        let if_pos = self.current_pos();
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::OpenPar)?;
        let bool_exp = self.parse_expression();
        self.expect(TokenKind::ClosePar)?;
        let (body, else_body) = if let Some(TokenKind::OpenCurB) = self.peek() {
            let body = self.parse_body()?;
            let else_body = if self.check(TokenKind::Else) {
                self.expect(TokenKind::Else)?;
                Some(self.parse_body()?)
            } else {
                None
            };

            (body, else_body)
        } else {
            let expression_pos = self.current_pos();
            (
                vec![Statement::from_pos(
                    StatementKind::Expression(self.parse_expression()?),
                    expression_pos,
                )],
                None,
            )
        };

        Ok(Statement::from_pos(
            StatementKind::If(IfStatement {
                boolean_op: bool_exp?,
                then_statements: body,
                else_statements: else_body,
            }),
            if_pos,
        ))
    }

    pub fn parse_for(&mut self) -> Result<Statement, CompilerError> {
        let for_pos = self.current_pos();
        self.expect(TokenKind::For)?;
        self.expect(TokenKind::OpenPar)?;
        let initialiser = self.parse_declaration()?;

        self.expect(TokenKind::Semicolon)?;
        let condition = self.parse_expression()?;

        self.expect(TokenKind::Semicolon)?;
        let accumalator = self.parse_expression()?;
        self.expect(TokenKind::ClosePar)?;

        let body = if let Some(TokenKind::OpenCurB) = self.peek() {
            self.parse_body()?
        } else {
            let expression_pos = self.current_pos();
            vec![Statement::from_pos(
                StatementKind::Expression(self.parse_expression()?),
                expression_pos,
            )]
        };

        Ok(Statement::from_pos(
            StatementKind::For(Box::new(ForStatement {
                initialiser,
                condition,
                accumalator,
                body,
            })),
            for_pos,
        ))
    }

    pub fn parse_top_level_declaration(&mut self) -> Result<Statement, CompilerError> {
        let pos = self.current_pos();
        match self.peek() {
            Some(&TokenKind::Function) => self.parse_function(),
            Some(&TokenKind::Class) => self.parse_class(),
            _ => Err(CompilerError::syntax_error(
                pos,
                "Expected top-level declaration here.",
            )),
        }
    }

    // FIXME: higher level declarations first?
    pub fn parse_function(&mut self) -> Result<Statement, CompilerError> {
        let function_pos = self.current_pos();
        let prototype = self.parse_prototype()?;
        self.expect(TokenKind::Arrow)?;
        let body = if let Some(TokenKind::OpenCurB) = self.peek() {
            self.parse_body()?
        } else {
            // TODO: Should this be a parse_statement?
            // Is `function main() => function hi() => {}`
            // valid?
            let expression_pos = self.current_pos();
            vec![Statement::from_pos(
                StatementKind::Expression(self.parse_expression()?),
                expression_pos,
            )]
        };
        Ok(Statement::from_pos(
            StatementKind::Function(Function { prototype, body }),
            function_pos,
        ))
    }

    pub fn parse_prototype(&mut self) -> Result<Prototype, CompilerError> {
        self.expect(TokenKind::Function)?;

        let Some(TokenKind::Symbol(function_name)) = self.next() else {
            return Err(CompilerError::syntax_error(
                self.previous_pos(),
                "Invalid state: expected to parse prototype, but did not find name, report this error",
            ));
        };

        self.expect(TokenKind::OpenPar)?;

        let mut arguments = Vec::new();
        while let Some(t) = self.next() {
            match t {
                TokenKind::ClosePar if self.check(TokenKind::Colon) => {
                    self.next();
                    return Ok(Prototype {
                        name: function_name,
                        arguments,
                        return_type: self.parse_type()?,
                    });
                }
                TokenKind::ClosePar => {
                    return Ok(Prototype {
                        name: function_name,
                        arguments,
                        return_type: Type::Void,
                    })
                }
                TokenKind::Symbol(arg) => {
                    self.expect(TokenKind::Colon)?;
                    arguments.push((arg, self.parse_type()?));
                    if self.check(TokenKind::ClosePar) {
                        continue;
                    } else {
                        self.expect(TokenKind::Comma)?;
                    }
                }
                t => {
                    return Err(CompilerError::SyntaxError(
                        self.previous_pos(),
                        format!("Expected symbol, found {:?}", t),
                    ));
                }
            }
        }

        Err(CompilerError::syntax_error(
            self.previous_pos(),
            "Unexpected end of file",
        ))
    }

    pub fn parse_body(&mut self) -> Result<Vec<Statement>, CompilerError> {
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
                    let expression_pos = self.current_pos();
                    statements.push(Statement::from_pos(
                        StatementKind::Expression(self.parse_expression()?),
                        expression_pos,
                    ));
                    self.consume_bang();
                }
            }
        }
        self.expect(TokenKind::CloseCurB)?;
        Ok(statements)
    }

    pub fn parse_mutable(&mut self) -> Result<Mutable, CompilerError> {
        let mut flags = Mutable::NONE;
        let pos = self.current_pos();
        let first_op = self.next().ok_or_else(|| {
            CompilerError::SyntaxError(
                self.current_pos(),
                String::from("Unexpected state: declaration found"),
            )
        })?;
        let second_op = self.next().ok_or_else(|| {
            CompilerError::SyntaxError(self.current_pos(), String::from("Unexpected end of file"))
        })?;
        const ALLOWED_TOKENS: [TokenKind; 2] = [TokenKind::Var, TokenKind::Const];
        if !ALLOWED_TOKENS.contains(&first_op) || !ALLOWED_TOKENS.contains(&second_op) {
            return Err(CompilerError::SyntaxError(
                pos,
                String::from("Declaration is missing var/const"),
            ));
        }
        if first_op == TokenKind::Var {
            flags |= Mutable::Reassignable;
        }
        if second_op == TokenKind::Var {
            flags |= Mutable::Modifiable;
        }
        Ok(flags)
    }

    pub fn parse_declaration(&mut self) -> Result<Statement, CompilerError> {
        let declaration_pos = self.current_pos();
        let flags = self.parse_mutable()?;

        let symbol_pos = self.current_pos();
        let expression = self.next();

        let Some(TokenKind::Symbol(lhs)) = expression else {
            return Err(CompilerError::SyntaxError(
                symbol_pos,
                format!("Expected identifier, found {:?} in declaration", expression),
            ));
        };

        let t = if let Some(TokenKind::Colon) = self.peek() {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };

        let eq_pos = self.current_pos();
        if let Some(TokenKind::Eq) = self.next() {
            let rhs = self.parse_expression();
            self.consume_bang();
            Ok(Statement::from_pos(
                StatementKind::Declaration(Declaration {
                    mutable: flags,
                    lhs,
                    rhs: rhs?,
                    var_type: t,
                }),
                declaration_pos,
            ))
        } else {
            Err(CompilerError::SyntaxError(
                eq_pos,
                String::from("Expected '=' in the declaration."),
            ))
        }
    }

    fn parse_return(&mut self) -> Result<Statement, CompilerError> {
        let return_pos = self.current_pos();
        self.expect(TokenKind::Return)?;
        let return_value = self.parse_expression().ok();
        // Should probably be in statement
        self.optional(TokenKind::Bang);
        Ok(Statement::from_pos(
            StatementKind::Return { return_value },
            return_pos,
        ))
    }

    fn parse_array(&mut self) -> Result<Expression, CompilerError> {
        self.expect(TokenKind::OpenSqB)?;
        let mut values = vec![];
        while !self.check(TokenKind::CloseSqB) {
            let expr = self.parse_expression()?;
            values.push(expr);
            match self.peek() {
                Some(TokenKind::Comma) => self.expect(TokenKind::Comma)?,
                Some(TokenKind::CloseSqB) => {}
                tkn => {
                    return Err(CompilerError::SyntaxError(
                        self.current_pos(),
                        format!(
                            "Expected comma or bracket, found {}",
                            tkn.map_or("none".to_string(), |f| format!("{:?}", f))
                        ),
                    ));
                }
            }
        }
        self.expect(TokenKind::CloseSqB)?;
        Ok(Expression::from_pos(
            ExpressionKind::Array(values),
            self.current_pos(),
        ))
    }

    pub fn parse_type(&mut self) -> Result<Type, CompilerError> {
        let symb = self.next();
        let Some(TokenKind::Symbol(t)) = symb else {
            return Err(CompilerError::syntax_error(
                self.previous_pos(),
                format!("Expected a type, found {:?}", symb),
            ));
        };
        let mut t = match t.as_str() {
            "int" => Type::Int,
            "short" => Type::Short,
            "long" => Type::Long,
            "byte" => Type::Byte,
            "float" => Type::Float,
            "double" => Type::Double,
            "void" => Type::Void,
            _ => {
                return Err(CompilerError::syntax_error(
                    self.previous_pos(),
                    format!("Type not found: {}", t),
                ))
            }
        };
        match self.peek() {
            Some(TokenKind::Star) => {
                self.next();
                t = Type::Pointer(Box::new(t));
            }
            Some(TokenKind::OpenSqB) => {
                self.next();
                let token = self.next();
                if let Some(TokenKind::Symbol(size)) = token {
                    let size: u32 = size.parse().map_err(|_| {
                        CompilerError::syntax_error(self.previous_pos(), "Expected size")
                    })?;
                    self.expect(TokenKind::CloseSqB)?;
                    t = Type::Array(Box::new(t), size);
                } else if let Some(TokenKind::CloseSqB) = token {
                    // FIXME: Is this an actually good idea?
                    // Semantically byte[] == byte*
                    t = Type::Pointer(Box::new(t));
                }
            }
            _ => {}
        }
        Ok(t)
    }

    fn parse_call(
        &mut self,
        callee: String,
        value_pos: SourcePosition,
    ) -> Result<Expression, CompilerError> {
        let mut args = Vec::new();
        while let Some(token) = self.peek() {
            if *token == TokenKind::ClosePar {
                self.next();
                return Ok(Expression::from_pos(
                    ExpressionKind::Call {
                        callee,
                        arguments: args,
                    },
                    value_pos,
                ));
            }

            args.push(self.parse_expression()?);

            if Some(&TokenKind::ClosePar) == self.peek() {
                continue;
            }

            parse_or!(
                self,
                TokenKind::Comma,
                return Err(CompilerError::SyntaxError(
                    self.current_pos(),
                    format!(
                        "Expected comma or closing bracked, found {:?}",
                        self.peek()
                            .map(|f| format!("{:?}", f))
                            .unwrap_or("none".to_string())
                    ),
                ))
            );
        }
        Ok(Expression::from_pos(
            ExpressionKind::Unknown,
            self.current_pos(),
        ))
    }
}
