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
    Equal,
    StrictEqual,
    VeryStrictEqual,
    Greater,
    Less,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
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
