use super::Parser;
use crate::{
    ast::{BinOperation, Expression, ExpressionKind, UnaryOperation},
    compile_error::CompilerError,
    lexer::TokenKind,
};

impl Parser {
    pub fn parse_equality(&mut self) -> Result<Expression, CompilerError> {
        let mut expr = self.parse_term()?;

        // FIXME: this pattern is pretty ugly
        while let Some(
            TokenKind::EqEq
            | TokenKind::EqEqEq
            | TokenKind::EqEqEqEq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::LtEq
            | TokenKind::GtEq,
        ) = self.peek()
        {
            let operation = match self.next().unwrap() {
                TokenKind::EqEq => BinOperation::Equal,
                TokenKind::EqEqEq => BinOperation::StrictEqual,
                TokenKind::EqEqEqEq => BinOperation::VeryStrictEqual,
                TokenKind::Lt => BinOperation::Less,
                TokenKind::Gt => BinOperation::Greater,
                TokenKind::LtEq => BinOperation::LessThanOrEqual,
                TokenKind::GtEq => BinOperation::GreaterThanOrEqual,
                _ => panic!("Invalid operation (the compiler should not do this)"),
            };
            let binary_pos = self.current_pos();
            let rhs = self.parse_term();
            expr = Expression::from_pos(
                ExpressionKind::Binary {
                    lhs: Box::new(expr),
                    operation,
                    rhs: Box::new(rhs?),
                },
                binary_pos,
            );
        }

        Ok(expr)
    }

    pub fn parse_term(&mut self) -> Result<Expression, CompilerError> {
        let mut expr = self.parse_factor()?;

        while let Some(TokenKind::Plus | TokenKind::Dash) = self.peek() {
            let operation = match self.next().unwrap() {
                TokenKind::Plus => BinOperation::Add,
                TokenKind::Dash => BinOperation::Subtract,
                _ => panic!("Invalid operation (the compiler should not do this)"),
            };
            let term_pos = self.current_pos();
            let rhs = self.parse_factor();

            expr = Expression::from_pos(
                ExpressionKind::Binary {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs?),
                    operation,
                },
                term_pos,
            );
        }

        Ok(expr)
    }

    pub fn parse_factor(&mut self) -> Result<Expression, CompilerError> {
        let factor_pos = self.current_pos();
        let mut expr = self.parse_remainder()?;

        while let Some(TokenKind::Star | TokenKind::Slash) = self.peek() {
            let operation = match self.next().unwrap() {
                TokenKind::Star => BinOperation::Multiply,
                TokenKind::Slash => BinOperation::Divide,
                _ => panic!("Invalid operation"),
            };
            let rhs = self.parse_remainder();

            expr = Expression::from_pos(
                ExpressionKind::Binary {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs?),
                    operation,
                },
                factor_pos,
            );
        }

        Ok(expr)
    }

    pub fn parse_remainder(&mut self) -> Result<Expression, CompilerError> {
        let remainder_pos = self.current_pos();
        let mut expr = self.parse_unary()?;

        while let Some(TokenKind::Percent) = self.peek() {
            let operation = match self.next().unwrap() {
                TokenKind::Percent => BinOperation::Remainder,
                _ => panic!("Invalid operation"),
            };
            let rhs = self.parse_unary();

            expr = Expression::from_pos(
                ExpressionKind::Binary {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs?),
                    operation,
                },
                remainder_pos,
            );
        }

        Ok(expr)
    }

    pub fn parse_unary(&mut self) -> Result<Expression, CompilerError> {
        let pos = self.current_pos();
        parse_or!(self, TokenKind::Dash, return self.parse_operators());

        let expr = self.parse_unary()?;
        Ok(Expression::from_pos(
            ExpressionKind::Unary {
                expression: Box::new(expr),
                operation: UnaryOperation::Negation,
            },
            pos,
        ))
    }

    pub fn parse_member(&mut self, expression: Expression) -> Result<Expression, CompilerError> {
        let current_pos = self.current_pos();
        if self.check(TokenKind::Dot) {
            self.expect(TokenKind::Dot)?;
            if let Some(TokenKind::Symbol(sym)) = self.next() {
                return Ok(Expression::from_pos(
                    ExpressionKind::Member(Box::new(expression), sym),
                    current_pos,
                ));
            } else {
                return Err(CompilerError::syntax_error(current_pos, "Expected member"));
            }
        }
        Ok(expression)
    }

    pub fn parse_reference_operators(&mut self) -> Result<Expression, CompilerError> {
        let current_pos = self.current_pos();
        let expression = match self.peek() {
            Some(TokenKind::Star) => {
                self.expect(TokenKind::Star)?;
                let expression = self.parse_reference_operators()?;
                Expression::from_pos(
                    ExpressionKind::Dereference(Box::new(expression)),
                    current_pos,
                )
            }
            Some(TokenKind::Ampersand) => {
                self.expect(TokenKind::Ampersand)?;
                let expression = self.parse_reference_operators()?;
                Expression::from_pos(ExpressionKind::Reference(Box::new(expression)), current_pos)
            }
            _ => self.parse_value()?,
        };
        Ok(expression)
    }

    pub fn parse_operators(&mut self) -> Result<Expression, CompilerError> {
        let pos = self.current_pos();
        let mut expr = self.parse_reference_operators()?;

        while let Some(TokenKind::OpenSqB | TokenKind::Dot) = self.peek() {
            if self.check(TokenKind::OpenSqB) {
                self.expect(TokenKind::OpenSqB)?;
                let index = self.parse_expression()?;
                self.expect(TokenKind::CloseSqB)?;
                expr = Expression::from_pos(
                    ExpressionKind::IndexOperator {
                        expression: Box::new(expr),
                        index: Box::new(index),
                    },
                    pos,
                );
            } else {
                expr = self.parse_member(expr)?;
            }
        }

        Ok(expr)
    }

    pub fn parse_value(&mut self) -> Result<Expression, CompilerError> {
        let value_pos = self.current_pos();
        if let Some(token) = self.next() {
            return match token {
                TokenKind::Symbol(sym) => {
                    parse_or!(
                        self,
                        TokenKind::OpenPar,
                        return Ok(Expression::from_pos(
                            ExpressionKind::Identifier(sym),
                            value_pos,
                        ))
                    );
                    return self.parse_call(sym, value_pos);
                }
                TokenKind::String(str) => Ok(Expression::from_pos(
                    ExpressionKind::LiteralValue(str),
                    self.current_pos(),
                )),
                TokenKind::New => {
                    let instance_type = self.parse_type()?;
                    return Ok(Expression::from_pos(
                        ExpressionKind::Instantiation(instance_type),
                        value_pos,
                    ));
                }
                TokenKind::OpenPar => {
                    let exp = self.parse_expression()?;
                    self.expect(TokenKind::ClosePar)?;
                    return Ok(exp);
                }
                tkn => Err(CompilerError::syntax_error(
                    self.previous_pos(),
                    format!("Expected expression, found {:?}", tkn),
                )),
            };
        }
        Err(CompilerError::syntax_error(
            self.current_pos(),
            "Expected expression, found none",
        ))
    }
}
