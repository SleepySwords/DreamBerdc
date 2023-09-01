use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
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

//     Unkown,
}

pub fn tokenize<T: Iterator<Item = char>>(tokens: &mut Peekable<T>) -> Vec<Token> {
    let mut ts = Vec::new();

    while let Some(token) = tokens.next() {
        let t = match token {
            '=' => equal(tokens),
            '(' => Token::OpenPar,
            ')' => Token::ClosePar,
            '[' => Token::OpenSqB,
            ']' => Token::CloseSqB,
            '{' => Token::OpenCurB,
            '}' => Token::CloseCurB,
            '.' => Token::Dot,
            '+' => Token::Plus,
            '-' => Token::Dash,
            '*' => Token::Star,
            '/' => Token::Slash,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '?' => Token::Question,
            '!' => Token::Bang,
            ',' => Token::Comma,
            '"' => string(tokens),
            _ => {
                if !token.is_alphanumeric() {
                    continue;
                }
                let mut word = String::new();
                word.push(token);
                while let Some(token) = tokens.peek() {
                    if !token.is_alphanumeric() {
                        break;
                    }
                    word.push(*token);
                    tokens.next();
                }
                match word.as_str() {
                    "const" => Token::Const,
                    "var" => Token::Var,
                    "return" => Token::Return,
                    "if" => Token::If,
                    i => {
                        if "function".contains(i) {
                            Token::Function
                        } else {
                            Token::Symbol(i.to_string())
                        }
                    }
                }
            }
        };
        ts.push(t)
    }
    ts.push(Token::Eof);
    ts
}

fn equal<T: Iterator<Item = char>>(tokens: &mut Peekable<T>) -> Token {
    if Some(&'>') == tokens.peek() {
        return Token::Arrow;
    }
    let mut eq_count = 1;
    while Some(&'=') == tokens.peek() {
        eq_count += 1;
        tokens.next();
    }
    match eq_count {
        1 => Token::Eq,
        2 => Token::EqEq,
        3 => Token::EqEqEq,
        4 => Token::EqEqEqEq,
        _ => {
            panic!("Too many eq")
        }
    }
}

fn string<T: Iterator<Item = char>>(tokens: &mut Peekable<T>) -> Token {
    let mut str = String::new();
    for token in tokens.by_ref() {
        if token == '"' {
            break;
        }
        str.push(token);
    }
    Token::String(str)
}
