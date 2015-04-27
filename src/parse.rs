use std::iter::Peekable;

use err::*;
use common::{ByteStr, Loc};
use lex::{Token, Span};

// AST nodes

pub trait Expr {
    fn a(&self) -> bool { true }
}

pub struct Assign {
    target: Box<Expr>,
    value: Box<Expr>,
}
impl Expr for Assign {}

pub struct Operator2 {
    op: ByteStr,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}
impl Expr for Operator2 {}

pub struct Param;
pub struct Proc {
    args: Vec<Param>,
    resty: Option<Box<Expr>>,
}
impl Expr for Proc {}


enum OpItem { Expr(Box<Expr>), Op(ByteStr) }

/// The actual parser. This object holds a tokens iterator (for example, Lex), and
/// will produce expressions when `parse` is called on it.
/// A program in aum is a list of expressions.
pub struct Parser<T: Iterator<Item = Span>> {
    tokens: Peekable<T>,
    toplevel: bool,
}

impl <T: Iterator<Item = Span>> Parser<T> {
    fn new(it: T) -> Parser<T> {
        Parser {
            tokens: it.peekable(),
            toplevel: true, // TODO(michael): Figure out the best way to implement this
        }
    }

    /// Peek at the next token in the input stream. Returns only the token (if it is present).
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().map(|&Span{ref tok, ..}| tok.clone())
    }

    /// Take the next token from the input stream. Returns only the token (if it is present).
    fn next(&mut self) -> Option<Token> {
        self.tokens.next().map(|Span{tok, ..}| tok)
    }

    /// Returns the current location of the parser input stream. (the start location of the next token).
    /// TODO(michael): This should probably return the start location of the most recently consumed token.
    fn loc(&mut self) -> Loc {
        if let Some(&Span{start, ..}) = self.tokens.peek() {
            start
        } else {
            Loc{ line: 0, col: 0 }
        }
    }

    fn parse(&mut self) -> AumResult<Box<Expr>> { self.parse_op() }

    fn parse_op(&mut self) -> AumResult<Box<Expr>> {
        let mut v = Vec::new();

        loop {
            match self.peek() {
                Some(Token::Op(ref bs)) => {
                    self.next();
                    v.push(OpItem::Op(bs.clone()));
                }

                Some(Token::LBrace) => { // [subscript]
                    unimplemented!()
                }

                Some(Token::LParen) => { // (fn call)
                    unimplemented!()
                }

                // These are valid end-of-expression characters
                Some(Token::Colon) |     // :
                Some(Token::Comma) |     // ,
                Some(Token::Semi) | // ;
                Some(Token::RParen) |    // )
                Some(Token::RBrace) |    // ]
                Some(Token::RBracket) |  // }
                Some(Token::LBracket) |  // {
                None => break,

                Some(_) => {
                    v.push(OpItem::Expr(try!(self.parse_atom())));
                }
            }
        }

        unimplemented!()

        // TODO(michael): Use precidence values to fold the expression correctly
    }

    /// Atoms are any non-operator items!
    fn parse_atom(&mut self) -> AumResult<Box<Expr>> {
        unimplemented!()
    }
}
