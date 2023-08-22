use std::cmp::Ordering;

use crate::utils::Mutable;

// Expressions return values, statements do not.
#[derive(Debug)]
#[allow(dead_code)]
pub enum Expression {
    BinaryExpression(BinaryExpression),
    CallExpression(CallExpression),
    Assignment(AssignmentExpression),
    LiteralValue(String),
    Identifier(String),
    Unkown,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl PartialOrd for Operation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        } else if (*self == Self::Multiply || *self == Self::Divide)
            && (*self == Self::Multiply || *self == Self::Divide)
        {
            return Some(Ordering::Equal);
        } else if (*self == Self::Add || *self == Self::Subtract)
            && (*self == Self::Add || *self == Self::Subtract)
        {
            return Some(Ordering::Equal);
        } else if (*self == Self::Multiply || *self == Self::Divide)
            && (*self == Self::Add || *self == Self::Subtract)
        {
            return Some(Ordering::Greater);
        } else if (*self == Self::Add || *self == Self::Subtract)
            && (*self == Self::Subtract || *self == Self::Divide)
        {
            return Some(Ordering::Greater);
        }
        unreachable!()
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct BinaryExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub operation: Operation,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct CallExpression {
    pub callee: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct AssignmentExpression {
    pub lhs: String,
    pub rhs: String,
}

#[derive(Debug)]
pub enum Statement {
    Declaration(Box<DeclarationStatement>),
    Function(Box<FunctionStatement>),
    Return(Box<ReturnStatement>),
    Expression(Expression),
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct DeclarationStatement {
    pub mutable: Mutable,
    pub lhs: String,
    pub rhs: Expression,
}

pub type Name = String;
pub type Type = String;

#[derive(Debug)]
#[allow(dead_code)]
pub struct Prototype {
    pub name: String,
    pub arguments: Vec<(Name, Type)>,
}

#[derive(Debug)]
pub struct FunctionStatement {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_value: Expression,
}
