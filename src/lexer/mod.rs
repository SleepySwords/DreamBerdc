use std::fmt::Debug;
use std::fmt::Display;

use crate::ast::Operation;

#[derive(PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lnum: usize,
    pub col: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lnum: usize, col: usize) -> Token {
        Token { kind, lnum, col }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{{}, {}}}", self.kind, self.lnum, self.col)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}, ({}, {})", self.kind, self.lnum, self.col)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Eof,
    Symbol(String),
    // Contains "", above cannot have spaces, this can.
    String(String),

    Bang,
    Question,
    Semicolon,
    Colon,
    Plus,
    PlusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Dash,
    DashEq,
    Percent,
    PercentEq,
    Eq,
    EqEq,
    EqEqEq,
    EqEqEqEq,
    Arrow,
    Dot,

    OpenPar,
    ClosePar,
    OpenSqB,
    CloseSqB,
    OpenCurB,
    CloseCurB,

    Comma,

    // Keywords
    Const,
    Var,
    Function,
    Return,
    If,
    Else,
    For,
    Lt,
    Gt,
    //     Unkown,
}

impl TokenKind {
    pub fn is_compound(&self) -> bool {
        [
            TokenKind::PlusEq,
            TokenKind::StarEq,
            TokenKind::DashEq,
            TokenKind::SlashEq,
            TokenKind::PercentEq,
        ]
        .contains(self)
    }

    pub fn operation_compound(&self) -> Option<Operation> {
        match self {
            Self::PlusEq => Some(Operation::Add),
            Self::StarEq => Some(Operation::Multiply),
            Self::DashEq => Some(Operation::Subtract),
            Self::SlashEq => Some(Operation::Divide),
            Self::PercentEq => Some(Operation::Remainder),
            _ => None,
        }
    }
}

pub struct Lexer {
    pub data: Vec<char>,
    pub pos: usize,
    pub current_lnum: usize,
    pub current_col: usize,
}

impl Lexer {
    pub fn new(data: Vec<char>) -> Lexer {
        Lexer {
            data,
            pos: 0,
            current_lnum: 0,
            current_col: 0,
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let ch = self.data.get(self.pos)?;
        if ch == &'\n' {
            self.current_col = 0;
            self.current_lnum += 1;
        } else {
            self.current_col += 1;
        }
        self.pos += 1;
        Some(*ch)
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.data.get(self.pos)
    }

    pub fn tokenise(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.next() {
            let lnum = self.current_lnum;
            let col = self.current_col;
            let token_kind = match token {
                '=' => self.equal(),
                '(' => TokenKind::OpenPar,
                ')' => TokenKind::ClosePar,
                '[' => TokenKind::OpenSqB,
                ']' => TokenKind::CloseSqB,
                '{' => TokenKind::OpenCurB,
                '}' => TokenKind::CloseCurB,
                '.' => TokenKind::Dot,
                '+' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        TokenKind::PlusEq
                    } else {
                        TokenKind::Plus
                    }
                }
                '-' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        TokenKind::DashEq
                    } else {
                        TokenKind::Dash
                    }
                }
                '*' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        TokenKind::StarEq
                    } else {
                        TokenKind::Star
                    }
                }
                '/' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        TokenKind::SlashEq
                    } else {
                        TokenKind::Slash
                    }
                }
                '%' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        TokenKind::PercentEq
                    } else {
                        TokenKind::Percent
                    }
                }
                ';' => TokenKind::Semicolon,
                ':' => TokenKind::Colon,
                '?' => TokenKind::Question,
                '!' => TokenKind::Bang,
                ',' => TokenKind::Comma,
                '>' => TokenKind::Gt,
                '<' => TokenKind::Lt,
                '"' => self.string(),
                _ => {
                    if !token.is_alphanumeric() || token.is_whitespace() {
                        continue;
                    }
                    let mut word = String::new();
                    word.push(token);
                    while let Some(token) = self.peek() {
                        // FIXME: this needs to be parsed above seperately (for method calls)
                        if (!token.is_alphanumeric() && *token != '.') || token.is_whitespace() {
                            break;
                        }
                        word.push(*token);
                        self.next();
                    }
                    match word.as_str() {
                        "const" => TokenKind::Const,
                        "var" => TokenKind::Var,
                        "return" => TokenKind::Return,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "for" => TokenKind::For,
                        i => {
                            if "function".contains(i) {
                                // NOTE: this should work with current functionality, however
                                // when adding new features that allow for a symbol followed
                                // by a letter (such as an infix function or in operators),
                                // would need to modify this
                                while self.peek().is_some_and(|x| x.is_whitespace()) {
                                    self.next();
                                }
                                if self.peek().is_some_and(|x| x.is_alphanumeric()) {
                                    TokenKind::Function
                                } else {
                                    TokenKind::Symbol(i.to_string())
                                }
                            } else {
                                TokenKind::Symbol(i.to_string())
                            }
                        }
                    }
                }
            };
            tokens.push(Token::new(token_kind, lnum, col - 1)) // as added by self.next() but i'm lazy
        }
        tokens.push(Token::new(
            TokenKind::Eof,
            self.current_lnum,
            self.current_col,
        ));
        tokens
    }

    fn equal(&mut self) -> TokenKind {
        let equal_pos = (self.current_col - 1, self.current_lnum);
        if Some(&'>') == self.peek() {
            self.next();
            return TokenKind::Arrow;
        }
        let mut eq_count = 1;
        while Some(&'=') == self.peek() {
            eq_count += 1;
            self.next();
        }
        match eq_count {
            1 => TokenKind::Eq,
            2 => TokenKind::EqEq,
            3 => TokenKind::EqEqEq,
            4 => TokenKind::EqEqEqEq,
            _ => {
                panic!("Too many eq ({}, {})", equal_pos.1 + 1, equal_pos.0 + 1)
            }
        }
    }

    fn string(&mut self) -> TokenKind {
        let mut str = String::new();
        loop {
            let token = self.next();
            if token == Some('"') {
                break;
            }
            if let Some(tk) = token {
                str.push(tk);
            } else {
                todo!("EOF")
            }
        }
        TokenKind::String(str)
    }
}
