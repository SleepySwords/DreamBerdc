use std::fmt::Debug;
use std::fmt::Display;

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
    Star,
    Slash,
    Dash,
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
        self.pos += 1;
        let ch = self.data.get(self.pos)?;
        if ch == &'\n' {
            self.current_col = 0;
            self.current_lnum += 1;
        } else {
            self.current_col += 1;
        }
        Some(*ch)
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.data.get(self.pos + 1)
    }

    pub fn tokenise(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.next() {
            let lnum = self.current_lnum;
            let col = if self.current_col > 0 {
                self.current_col - 1
            } else {
                self.current_col
            }; // as added by self.next() but i'm lazy
            let token_kind = match token {
                '=' => self.equal(),
                '(' => TokenKind::OpenPar,
                ')' => TokenKind::ClosePar,
                '[' => TokenKind::OpenSqB,
                ']' => TokenKind::CloseSqB,
                '{' => TokenKind::OpenCurB,
                '}' => TokenKind::CloseCurB,
                '.' => TokenKind::Dot,
                '+' => TokenKind::Plus,
                '-' => TokenKind::Dash,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                ';' => TokenKind::Semicolon,
                ':' => TokenKind::Colon,
                '?' => TokenKind::Question,
                '!' => TokenKind::Bang,
                ',' => TokenKind::Comma,
                '>' => TokenKind::Gt,
                '<' => TokenKind::Lt,
                '"' => self.string(),
                _ => {
                    if !token.is_alphanumeric() {
                        continue;
                    }
                    let mut word = String::new();
                    word.push(token);
                    while let Some(token) = self.peek() {
                        // FIXME: this needs to be parsed above seperately (for method calls)
                        if !token.is_alphanumeric() && *token != '.' {
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
            tokens.push(Token::new(token_kind, lnum, col))
        }
        tokens.push(Token::new(
            TokenKind::Eof,
            self.current_lnum,
            self.current_col,
        ));
        tokens
    }

    fn equal(&mut self) -> TokenKind {
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
                panic!("Too many eq")
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
