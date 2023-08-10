use crate::utils::Mutable;


// Expressions return values, statements do not.
#[derive(Debug)]
#[allow(dead_code)]
pub enum Expression {
    BinaryExpresstion(BinaryExpression),
    CallExpression(CallExpression),
    Assignment(AssignmentExpression),
    LiteralValue(String),
    Identifier(String),
    Unkown,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct BinaryExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
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
    Expression(Expression)
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct DeclarationStatement {
    pub mutable: Mutable,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Prototype {
    pub name: String,
    pub arguments: Vec<String>,
}

#[derive(Debug)]
pub struct FunctionStatement {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}
