use std::io::prelude::*;
use std::iter::Peekable;

use common::{ByteStr, Loc};

#[derive(Debug)]
pub struct Span {
    pub tok: Token,

    // The start loc is inclusive, but the end loc is not
    pub start: Loc,
    pub end: Loc,
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
    err: bool,
    pub loc: Loc,
}

impl <T: Iterator<Item = u8>> Lexer<T> {
    pub fn new(bytes: T) -> Lexer<T> {
        Lexer {
            bytes: bytes.peekable(),
            err: false,
            loc: Loc { line: 1, col: 0 }
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        let next = self.bytes.next();
        if let Some(b'\n') = next {
            self.loc.line += 1;
            self.loc.col = 0;
        } else {
            self.loc.col += 1;
        }
        next
    }

    fn peek_byte(&mut self) -> Option<&u8> {
        self.bytes.peek()
    }

    fn problem(&mut self, message: &str) -> Option<Result<Span, String>> {
        let prob = format!("{} at {}", message, self.loc);
        self.err = true;
        Some(Err(prob))
    }

    fn unexpected_eof(&mut self, reason: &str) -> Option<Result<Span, String>> {
        self.problem(&format!("Unexpected End of File {}", reason))
    }
}

impl <T: Iterator<Item = u8>> Iterator for Lexer<T> {
    type Item = Result<Span, String>;

    fn next(&mut self) -> Option<Result<Span, String>> {
        if self.err { return None } // We ran into a problem lexing

        loop {
            let start = self.loc;

            let next = self.next_byte();
            let tok = if let Some(c) = next {
                match c {
                    // Skip whitespace
                    b' ' | b'\t' | b'\n' => continue,

                    b'{' => Token::LBracket,
                    b'}' => Token::RBracket,
                    b'(' => Token::LParen,
                    b')' => Token::RParen,
                    b'[' => Token::LBrace,
                    b']' => Token::RBrace,
                    b'<' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Token::Lte
                            }
                            Some(&b'-') => {
                                self.next_byte();
                                Token::LeftArrow
                            }
                            _ => Token::Lt
                        }
                    }
                    b'>' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Token::Gte
                            }
                            _ => Token::Gt
                        }
                    }


                    b'!' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Token::NotEq
                            }
                            _ => Token::Bang
                        }
                    }
                    b'@' => Token::At,
                    b'#' => Token::Hash,
                    b'$' => Token::Dollar,
                    b'%' => Token::Percent,
                    b'^' => Token::Caret,
                    b'&' => {
                        match self.peek_byte() {
                            Some(&b'&') => {
                                self.next_byte();
                                Token::AndAnd
                            }
                            _ => Token::And
                        }
                    }
                    b'*' => Token::Star,
                    b'-' => {
                        match self.peek_byte() {
                            Some(&b'>') => {
                                self.next_byte();
                                Token::RightArrow
                            }
                            _ => Token::Minus
                        }
                    }
                    b'+' => Token::Plus,
                    b'_' => Token::Underscore,
                    b'=' => {
                        match self.peek_byte() {
                            Some(&b'=') => {
                                self.next_byte();
                                Token::EqEq
                            }
                            Some(&b'>') => {
                                self.next_byte();
                                Token::FatArrow
                            }
                            _ => Token::Equals
                        }
                    }
                    b'/' => {
                        match self.peek_byte() {
                            Some(&b'/') => { // Comments
                                // Skip the comment, and then read another token
                                loop {
                                    match self.next_byte() {
                                        Some(b'\n') | None => break,
                                        _ => {/* nop */}
                                    }
                                }
                                continue
                            }
                            Some(&b'*') => {
                                // Skip the comment, and then read another token
                                loop {
                                    match self.next_byte() {
                                        None => return self.unexpected_eof("while reading multiline comment"),
                                        Some(b'*') => {
                                            if let Some(&b'/') = self.peek_byte() {
                                                self.next_byte();
                                                break
                                            }
                                        },
                                        _ => {/* nop */}
                                    }
                                }
                                continue
                            }
                            _ => Token::Slash,
                        }
                    }
                    b'|' => {
                        match self.peek_byte() {
                            Some(&b'|') => {
                                self.next_byte();
                                Token::OrOr
                            }
                            _ => Token::Bar
                        }
                    }
                    b'?' => Token::Qmark,
                    b',' => Token::Comma,
                    b'.' => Token::Dot,
                    b'~' => Token::Tilde,
                    b':' => {
                        match self.peek_byte() {
                            Some(&b':') => {
                                self.next_byte();
                                Token::ColonColon
                            }
                            _ => Token::Colon
                        }
                    }
                    b';' => Token::Semi,

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

                        Token::String(ByteStr(val))
                    }

                    b'0'...b'9' => { // Numbers
                        let mut val = Vec::new();
                        val.push(c);
                        let mut seen_dot = false;
                        loop {
                            // TODO(michael): This is a really really terrible
                            // number parser, which does a bad job
                            // I should really deal with it much better
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
                        Token::Number(ByteStr(val))
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
                            b"if" => Token::If,
                            b"else" => Token::Else,
                            b"struct" => Token::Struct,
                            b"enum" => Token::Enum,

                            _ => Token::Ident(ByteStr(val))
                        }
                    }

                    _ => return self.problem(&format!("Illegal byte in input: {:?}", c))
                }
            } else {
                return None // EOF
            };
            let end = self.loc;

            // We've got a token, return it
            return Some(Ok(Span {
                tok: tok,
                start: start,
                end: end,
            }))
        }
    }

    // We have no idea how many values we're going to emit unfortunately :-/
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}
