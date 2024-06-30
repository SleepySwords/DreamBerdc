use super::Parser;
use crate::{
    ast::{Class, FieldDeclaration, Statement, StatementKind},
    compile_error::CompilerError,
    lexer::TokenKind,
};
impl Parser {
    pub fn parse_class(&mut self) -> Result<Statement, CompilerError> {
        let pos = self.current_pos();
        let mut fields = vec![];
        self.expect(TokenKind::Class)?;
        let Some(TokenKind::Symbol(name)) = self.next() else {
            return Err(CompilerError::syntax_error(pos, "Invalid name"));
        };
        self.expect(TokenKind::OpenCurB)?;
        while !self.check(TokenKind::CloseCurB) {
            fields.push(self.parse_field_declaration()?);
        }
        self.expect(TokenKind::CloseCurB)?;
        Ok(Statement::from_pos(
            StatementKind::Class(Class { name, fields }),
            pos,
        ))
    }

    pub fn parse_field_declaration(&mut self) -> Result<FieldDeclaration, CompilerError> {
        // FIXME: add visibility stuff as well...
        let decl_pos = self.current_pos();
        let flags = self.parse_mutable()?;
        let Some(TokenKind::Symbol(name)) = self.next() else {
            return Err(CompilerError::syntax_error(
                decl_pos,
                "Expected declaration name",
            ));
        };

        self.expect(TokenKind::Colon)?;
        let field_type = self.parse_type()?;

        let initialiser = if self.check(TokenKind::Eq) {
            self.expect(TokenKind::Eq)?;
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume_bang();

        Ok(FieldDeclaration {
            name,
            field_type,
            initialiser,
            mutable: flags,
            visibility: crate::ast::Visibility::Public,
        })
    }
}
