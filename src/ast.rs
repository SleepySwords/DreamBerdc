use crate::{types::Type, utils::Mutable};

/// (Column, Line number)
pub type SourcePosition = (usize, usize);

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub struct Expression {
    pub kind: ExpressionKind,
    pub lnum: usize,
    pub col: usize,
}

impl Expression {
    pub(crate) fn from_pos(kind: ExpressionKind, (col, lnum): SourcePosition) -> Expression {
        Expression { kind, lnum, col }
    }
    pub(crate) fn pos(&self) -> SourcePosition {
        (self.col, self.lnum)
    }
}

// Expressions return values, statements do not.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
#[allow(dead_code)]
pub enum ExpressionKind {
    Binary {
        lhs: Box<Expression>,
        operation: BinOperation,
        rhs: Box<Expression>,
    },
    Unary {
        operation: UnaryOperation,
        expression: Box<Expression>,
    },
    Call {
        callee: String,
        arguments: Vec<Expression>,
    },
    Assignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Dereference(Box<Expression>),
    Reference(Box<Expression>),
    IndexOperator {
        expression: Box<Expression>,
        index: Box<Expression>,
    },
    LiteralValue(String),
    Identifier(String),
    Array(Vec<Expression>),
    Instantiation(Type),

    Member(Box<Expression>, String),

    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub enum BinOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    StrictEqual,
    VeryStrictEqual,
    Greater,
    GreaterThanOrEqual,
    Less,
    LessThanOrEqual,
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub enum UnaryOperation {
    Negation,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub struct Statement {
    pub kind: StatementKind,
    pub lnum: usize,
    pub col: usize,
}

impl Statement {
    pub(crate) fn from_pos(kind: StatementKind, (col, lnum): SourcePosition) -> Statement {
        Statement { kind, lnum, col }
    }

    pub fn pos(&self) -> SourcePosition {
        (self.col, self.lnum)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub enum StatementKind {
    Declaration(Declaration),
    If(IfStatement),
    For(Box<ForStatement>),
    Function(Function),
    Class(Class), // FIXME: while this is supported in JS, there is debate whether this is actually
    // valid.
    Return { return_value: Option<Expression> },
    Free(Box<Expression>),
    Expression(Expression),
    Extern(Prototype),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
#[allow(dead_code)]
pub struct Declaration {
    pub mutable: Mutable,
    pub var_type: Option<Type>,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
#[allow(dead_code)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_statements: Vec<Statement>,
    pub else_statements: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
#[allow(dead_code)]
pub struct ForStatement {
    pub initialiser: Statement,
    pub condition: Expression,
    pub accumalator: Expression,
    pub body: Vec<Statement>,
}

pub type Name = String;

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
#[allow(dead_code)]
pub struct Prototype {
    pub name: String,
    pub arguments: Vec<(Name, Type)>,
    pub return_type: Type,
    pub is_var_args: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub struct Class {
    pub fields: Vec<FieldDeclaration>,
    pub(crate) name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub struct FieldDeclaration {
    pub name: String,
    pub mutable: Mutable,
    pub field_type: Type,
    pub initialiser: Option<Expression>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub enum Visibility {
    Public,
    Private,
    Protected,
}
