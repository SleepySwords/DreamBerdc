use crate::utils::Mutable;

// Expressions return values, statements do not.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expression {
    Binary(BinaryOp),
    Call(CallOp),
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
}

// impl PartialOrd for Operation {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         if self == other {
//             return Some(Ordering::Equal);
//         } else if (*self == Self::Multiply || *self == Self::Divide)
//             && (*other == Self::Multiply || *other == Self::Divide)
//         {
//             return Some(Ordering::Equal);
//         } else if (*self == Self::Add || *self == Self::Subtract)
//             && (*other == Self::Add || *other == Self::Subtract)
//         {
//             return Some(Ordering::Equal);
//         } else if (*self == Self::Multiply || *self == Self::Divide)
//             && (*other == Self::Add || *other == Self::Subtract)
//         {
//             return Some(Ordering::Greater);
//         } else if (*self == Self::Add || *self == Self::Subtract)
//             && (*other == Self::Subtract || *other == Self::Divide)
//         {
//             return Some(Ordering::Less);
//         }
//         unreachable!()
//     }
// }

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct BinaryOp {
    pub lhs: Box<Expression>,
    pub operation: Operation,
    pub rhs: Box<Expression>,
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
    Declaration(Box<DeclarationStatement>),
    Function(Box<FunctionStatement>),
    Return(Box<ReturnStatement>),
    Expression(Expression),
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct DeclarationStatement {
    pub mutable: Mutable,
    pub lhs: String,
    pub rhs: Expression,
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
pub struct FunctionStatement {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}
