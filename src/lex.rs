use std::fmt;
use std::io::prelude::*;
use std::iter::Peekable;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ByteStr(Vec<u8>);

impl fmt::Debug for ByteStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ByteStr(ref bytes) = *self;
        write!(f, "{:?}", String::from_utf8_lossy(bytes))
    }
}

#[derive(Debug)]
pub enum Token {
    // {} () [] < >
    LBracket,
    RBracket,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Lt,
    Gt,

    // ! @ # $ % ^ & * - + _ = / | ? , . ~ : ;
    Bang,
    At,
    Hash,
    Dollar,
    Percent,
    Caret,
    And,
    Star,
    Minus,
    Plus,
    Underscore,
    Equals,
    Slash,
    Bar,
    Qmark,
    Comma,
    Dot,
    Tilde,
    Colon,
    Semi,

    // Multicharacter tokens
    // == <= >= !=
    EqEq,
    Lte,
    Gte,
    NotEq,
    // && ||
    AndAnd,
    OrOr,
    // ::
    ColonColon,
    // ->, <-, =>
    RightArrow,
    LeftArrow,
    FatArrow,

    // Compound Tokens
    Ident(ByteStr), // These _should_ be interned, but they aren't
    Number(ByteStr), // For now, I will store numbers as u8 vectors - I'll deal with the rest later
    String(ByteStr),

    // Identifiers
    If,
    Else,
    Struct,
    Enum,
}

pub struct Lexer<T: Iterator<Item = u8>> {
    bytes: Peekable<T>,
    pub problem: Option<String>,
    pub line: usize,
    pub col: usize
}

impl <T: Iterator<Item = u8>> Lexer<T> {
    pub fn new(bytes: T) -> Lexer<T> {
        Lexer {
            bytes: bytes.peekable(),
            problem: None,
            line: 1,
            col: 0
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        let next = self.bytes.next();
        if let Some(b'\n') = next {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        next
    }

    fn peek_byte(&mut self) -> Option<&u8> {
        self.bytes.peek()
    }

    fn problem(&mut self, message: &str) -> Option<Token> {
        self.problem = Some(format!("{} at line {}, column: {}", message, self.line, self.col));
        None
    }

    fn unexpected_eof(&mut self, reason: &str) -> Option<Token> {
        self.problem(&format!("Unexpected End of File {}", reason));
        None
    }
}

impl <T: Iterator<Item = u8>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.problem.is_some() { return None } // We ran into a problem lexing

        loop {
            let next = self.next_byte();

            return if let Some(c) = next {
                match c {
                    // Skip whitespace
                    b' ' | b'\t' | b'\n' => continue,

                    b'{' => Some(Token::LBracket),
                    b'}' => Some(Token::RBracket),
                    b'(' => Some(Token::LParen),
                    b')' => Some(Token::RParen),
                    b'[' => Some(Token::LBrace),
                    b']' => Some(Token::RBrace),
                    b'<' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Some(Token::Lte)
                            }
                            Some(&b'-') => {
                                self.next_byte();
                                Some(Token::LeftArrow)
                            }
                            _ => Some(Token::Lt)
                        }
                    }
                    b'>' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Some(Token::Gte)
                            }
                            _ => Some(Token::Gt)
                        }
                    }


                    b'!' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Some(Token::NotEq)
                            }
                            _ => Some(Token::Bang)
                        }
                    }
                    b'@' => Some(Token::At),
                    b'#' => Some(Token::Hash),
                    b'$' => Some(Token::Dollar),
                    b'%' => Some(Token::Percent),
                    b'^' => Some(Token::Caret),
                    b'&' => {
                        match self.peek_byte() {
                            Some(&b'&') => {
                                self.next_byte();
                                Some(Token::AndAnd)
                            }
                            _ => Some(Token::And)
                        }
                    }
                    b'*' => Some(Token::Star),
                    b'-' => {
                        match self.peek_byte() {
                            Some(&b'>') => {
                                self.next_byte();
                                Some(Token::RightArrow)
                            }
                            _ => Some(Token::Minus)
                        }
                    }
                    b'+' => Some(Token::Plus),
                    b'_' => Some(Token::Underscore),
                    b'=' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Some(Token::EqEq)
                            }
                            Some(&b'>') => {
                                self.next_byte();
                                Some(Token::FatArrow)
                            }
                            _ => Some(Token::Equals)
                        }
                    }
                    b'/' => Some(Token::Slash),
                    b'|' => {
                        match self.peek_byte() {
                            Some(&b'|') => {
                                self.next_byte();
                                Some(Token::OrOr)
                            }
                            _ => Some(Token::Bar)
                        }
                    }
                    b'?' => Some(Token::Qmark),
                    b',' => Some(Token::Comma),
                    b'.' => Some(Token::Dot),
                    b'~' => Some(Token::Tilde),
                    b':' => {
                        match self.peek_byte() {
                            Some(&b':') => {
                                self.next_byte();
                                Some(Token::ColonColon)
                            }
                            _ => Some(Token::Colon)
                        }
                    }
                    b';' => Some(Token::Semi),

                    b'"' => {
                        // Strings
                        let mut val = Vec::new();
                        loop {
                            match self.next_byte() {
                                Some(b'"') => break,
                                Some(b'\\') => {
                                    match self.next_byte() {
                                        Some(b'n') => val.push(b'\n'),
                                        Some(b'r') => val.push(b'\r'),
                                        Some(c) => val.push(c),
                                        None => return self.unexpected_eof("while parsing string literal")
                                    }
                                }
                                Some(c) => val.push(c),
                                None => return self.unexpected_eof("while parsing string literal"),
                            }
                        }

                        Some(Token::String(ByteStr(val)))
                    }

                    b'0'...b'9' => { // Numbers
                        let mut val = Vec::new();
                        val.push(c);
                        let mut seen_dot = false;
                        loop {
                            match self.peek_byte() {
                                Some(&b'0'...b'9') => {
                                    val.push(self.next_byte().unwrap());
                                }
                                Some(&b'.') => {
                                    self.next_byte();
                                    if seen_dot {
                                        return self.problem("Unexpected '.' character in numeric literal")
                                    } else {
                                        val.push(b'.');
                                        seen_dot = true;
                                    }
                                }
                                _ => break
                            }
                        }
                        Some(Token::Number(ByteStr(val)))
                    }

                    b'a'...b'z' | b'A'...b'Z' => {
                        // Identifiers (currently only ascii chars allowed)
                        let mut val = Vec::new();
                        val.push(c);
                        loop {
                            match self.peek_byte() {
                                Some(&b'a'...b'z') |
                                Some(&b'A'...b'Z') |
                                Some(&b'_') |
                                Some(&b'0'...b'9') => {
                                    val.push(self.next_byte().unwrap());
                                }
                                _ => break
                            }
                        }

                        // Screen for keywords
                        match &val[..] {
                            b"if" => Some(Token::If),
                            b"else" => Some(Token::Else),
                            b"struct" => Some(Token::Struct),
                            b"enum" => Some(Token::Enum),

                            _ => Some(Token::Ident(ByteStr(val)))
                        }
                    }

                    _ => self.problem(&format!("Illegal byte in input: {:?}", c))
                }
            } else {
                None // EOF
            }
        }
    }

    // We have no idea how many values we're going to emit unfortunately :-/
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}
