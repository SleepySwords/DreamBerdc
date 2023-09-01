use crate::utils::Mutable;

// Expressions return values, statements do not.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expression {
    Binary {
        lhs: Box<Expression>,
        operation: Operation,
        rhs: Box<Expression>,
    },
    Call(CallOp),
    // Why is assignment here, that makes no sense.
    Assignment(AssignmentOp),
    LiteralValue(String),
    Identifier(String),
    Unkown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equality,
    StrictEquality,
    VeryStrictEquality,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct CallOp {
    pub callee: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct AssignmentOp {
    pub lhs: String,
    pub rhs: String,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Box<Declaration>),
    If(Box<IfStatement>),
    Function(Box<Function>),
    Return{
        return_value: Box<Expression>
    },
    Expression(Expression),
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Declaration {
    pub mutable: Mutable,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct IfStatement {
    pub boolean_op: Expression,
    pub then_statemenets: Vec<Statement>,
    pub else_statements: Option<Vec<Statement>>,
}

pub type Name = String;
pub type Type = String;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Prototype {
    pub name: String,
    pub arguments: Vec<(Name, Type)>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}
