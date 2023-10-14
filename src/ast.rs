use crate::{types::Type, utils::Mutable};

// Expressions return values, statements do not.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Expression {
    Binary {
        lhs: Box<Expression>,
        operation: Operation,
        rhs: Box<Expression>,
    },
    Call(Call),
    Assignment(Assignment),
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
    Equal,
    StrictEqual,
    VeryStrictEqual,
    Greater,
    Less,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Call {
    pub callee: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Assignment {
    pub lhs: String,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Declaration(Box<Declaration>),
    If(Box<IfStatement>),
    For(Box<ForStatement>),
    Function(Box<Function>),
    Return { return_value: Box<Expression> },
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Declaration {
    pub mutable: Mutable,
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
