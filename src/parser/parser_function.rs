use super::Parser;

use crate::{
    ast::{Function, Prototype, Statement, StatementKind},
    compile_error::CompilerError,
    lexer::TokenKind,
    types::Type,
};

impl Parser {
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

    pub fn parse_return(&mut self) -> Result<Statement, CompilerError> {
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
}
