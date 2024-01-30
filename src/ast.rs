use crate::{types::Type, utils::Mutable};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub lnum: usize,
    pub col: usize,
}
impl Expression {
    pub(crate) fn from_pos(kind: ExpressionKind, (col, lnum): (usize, usize)) -> Expression {
        Expression { kind, lnum, col }
    }
}

// Expressions return values, statements do not.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ExpressionKind {
    Binary {
        lhs: Box<Expression>,
        operation: Operation,
        rhs: Box<Expression>,
    },
    Call {
        callee: String,
        arguments: Vec<Expression>,
    },
    Assignment {
        lhs: String,
        rhs: Box<Expression>,
    },
    LiteralValue(String),
    Identifier(String),
    Array(Vec<Expression>),
    Unkown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operation {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
    pub kind: StatementKind,
    pub lnum: usize,
    pub col: usize,
}

impl Statement {
    pub(crate) fn from_pos(kind: StatementKind, (col, lnum): (usize, usize)) -> Statement {
        Statement { kind, lnum, col }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
    Declaration(Declaration),
    If(IfStatement),
    For(Box<ForStatement>),
    Function(Function),
    Return { return_value: Expression },
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Declaration {
    pub mutable: Mutable,
    pub var_type: Option<Type>,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct IfStatement {
    pub boolean_op: Expression,
    pub then_statements: Vec<Statement>,
    pub else_statements: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct ForStatement {
    pub initialiser: Statement,
    pub condition: Expression,
    pub accumalator: Expression,
    pub body: Option<Vec<Statement>>,
}

pub type Name = String;

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Prototype {
    pub name: String,
    pub arguments: Vec<(Name, Type)>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}
