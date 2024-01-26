use crate::{types::Type, utils::Mutable};


pub struct Expression {
    pub kind: ExpressionKind,
    pub lnum: usize,
    pub col: usize,
}

// Expressions return values, statements do not.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ExpressionKind {
    Binary {
        lhs: Box<ExpressionKind>,
        operation: Operation,
        rhs: Box<ExpressionKind>,
    },
    Call {
        callee: String,
        arguments: Vec<ExpressionKind>,
    },
    Assignment {
        lhs: String,
        rhs: Box<ExpressionKind>,
    },
    LiteralValue(String),
    Identifier(String),
    Array(Vec<ExpressionKind>),
    Unkown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    StrictEqual,
    VeryStrictEqual,
    Greater,
    Less,
}

pub struct Statement {
    pub kind: StatementKind,
    pub lnum: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
    Declaration(Declaration),
    If(IfStatement),
    For(Box<ForStatement>),
    Function(Function),
    Return { return_value: ExpressionKind },
    Expression(ExpressionKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Declaration {
    pub mutable: Mutable,
    pub var_type: Option<Type>,
    pub lhs: String,
    pub rhs: ExpressionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct IfStatement {
    pub boolean_op: ExpressionKind,
    pub then_statements: Vec<StatementKind>,
    pub else_statements: Option<Vec<StatementKind>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct ForStatement {
    pub initialiser: StatementKind,
    pub condition: ExpressionKind,
    pub accumalator: ExpressionKind,
    pub body: Option<Vec<StatementKind>>,
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
    pub body: Vec<StatementKind>,
}
