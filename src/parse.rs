use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Peekable;
use std::mem::swap;

use err::*;
use common::{ByteStr, Loc};
use lex::{Token, Span};

// AST nodes

pub trait Expr: Debug {
    fn a(&self) -> bool { true }
}

#[derive(Debug)]
pub struct Assign {
    target: Box<Expr>,
    value: Box<Expr>,
}
impl Expr for Assign {}

#[derive(Debug)]
pub struct Operator2 {
    op: ByteStr,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}
impl Expr for Operator2 {}

#[derive(Debug)]
pub struct Param;

#[derive(Debug)]
pub struct Proc {
    args: Vec<Param>,
    resty: Option<Box<Expr>>,
}
impl Expr for Proc {}

#[derive(Debug)]
enum OpTreeNode { // TODO(michael): Not a super efficient data structure
    Prefix {
        op: ByteStr,
        exp: Option<Box<OpTreeNode>>,
    },
    Postfix {
        op: ByteStr,
        exp: Option<Box<OpTreeNode>>,
    },
    LeftAssoc {
        op: ByteStr,
        lhs: Option<Box<OpTreeNode>>,
        rhs: Option<Box<OpTreeNode>>,
    },
    RightAssoc {
        op: ByteStr,
        lhs: Option<Box<OpTreeNode>>,
        rhs: Option<Box<OpTreeNode>>,
    },
    Expr {
        val: Box<Expr>,
    }
}
impl OpTreeNode {
    fn precidence(&self) -> i32 {
        unimplemented!()
    }

    fn append(&mut self, mut new: Box<OpTreeNode>) -> AumResult<()> {
        use self::OpTreeNode::*;

        let prec = self.precidence();

        match *self {
            Prefix{ref mut exp, ..} => {
                if let Some(ref mut tn) = *exp {
                    // TODO(michael): Implement more correctly
                    try!(tn.append(new));
                } else {
                    match *new {
                        Prefix{..} | Expr{..} => *exp = Some(new),
                        _ => return aum_err!("Prefix expression without body"),
                    }
                }
            }

            Expr{..} | Postfix{..} => {
                // These values are effectively indivisible
                match *new {
                    // Right and left assoc are treated the same because this is
                    // the initial insertion
                    RightAssoc{..} | LeftAssoc{..} | Postfix{..} => {
                        // The new root is the just-read-in value
                        swap(self, &mut *new);
                        match *self {
                            RightAssoc{ref mut lhs, ..} => *lhs = Some(new),
                            LeftAssoc{ref mut lhs, ..} => *lhs = Some(new),
                            Postfix{ref mut exp, ..} => *exp = Some(new),
                            _ => unreachable!()
                        }
                    }

                    Prefix{..} => return aum_err!("Unexpected prefix operator"),
                    Expr{..} => return aum_err!("Unexpected expression"),
                }
            }

            RightAssoc{rhs: None, ..} | LeftAssoc{rhs: None, ..} => {
                match *new {
                    Prefix{..} | Expr{..} => {
                        match *self {
                            RightAssoc{ref mut rhs, ..} => *rhs = Some(new),
                            LeftAssoc{ref mut rhs, ..} => *rhs = Some(new),
                            _ => unreachable!()
                        }
                    }
                    RightAssoc{..} | LeftAssoc{..} =>
                        return aum_err!("Unexpected infix operator"),
                    Postfix{..} =>
                        return aum_err!("Unexpected postfix operator"),
                }
            }

            // a + b + c => a + (b + c)
            RightAssoc{rhs: Some(_), ..} => {
                if new.precidence() >= prec {
                    // a $ (b % c)
                    if let RightAssoc{rhs: Some(ref mut rhs), ..} = *self {
                        try!(rhs.append(new));
                    } else { unreachable!() }
                } else {
                    // (a $ b) % c
                    swap(self, &mut *new);
                    match *self {
                        Prefix{..} =>
                            return aum_err!("Unexpected prefix operator"),
                        Expr{..} =>
                            return aum_err!("Unexpected expression"),
                        Postfix{ref mut exp, ..} => *exp = Some(new),
                        RightAssoc{ref mut lhs, ..} => *lhs = Some(new),
                        LeftAssoc{ref mut lhs, ..} => *lhs = Some(new),
                    }
                }
            }

            // a + b + c => (a + b) + c
            LeftAssoc{rhs: Some(_), ..} => {
                if new.precidence() <= prec {
                    // (a $ b) % c
                    swap(self, &mut *new);
                    match *self {
                        Prefix{..} =>
                            return aum_err!("Unexpected prefix operator"),
                        Expr{..} =>
                            return aum_err!("Unexpected expression"),
                        Postfix{ref mut exp, ..} => *exp = Some(new),
                        RightAssoc{ref mut lhs, ..} => *lhs = Some(new),
                        LeftAssoc{ref mut lhs, ..} => *lhs = Some(new),
                    }
                } else {
                    // a $ (b % c)
                    if let LeftAssoc{rhs: Some(ref mut rhs), ..} = *self {
                        try!(rhs.append(new));
                    } else { unreachable!() }
                }
            }
        }

        Ok(())
    }
}


/// The actual parser. This object holds a tokens iterator (for example, Lex), and
/// will produce expressions when `parse` is called on it.
/// A program in aum is a list of expressions.
pub struct Parser<T: Iterator<Item = AumResult<Span>>> {
    tokens: Peekable<T>,

    operators: HashMap<ByteStr, OpTreeNode>, // Prototypical OpTreeNodes

    toplevel: bool,
}

impl <T: Iterator<Item = AumResult<Span>>> Parser<T> {
    fn new(it: T) -> Parser<T> {
        let parser = Parser {
            tokens: it.peekable(),
            operators: HashMap::new(),
            toplevel: true, // TODO(michael): Figure out the best way to implement this
        };

        // TODO(michael): For now I'm kinda copying Swift's precidence numbers
        // At some point I may change this to be something else
        // Dereference-level
        parser.add_infixl(
            ByteStr::from(b"."), 300);
        parser.add_infixl(
            ByteStr::from(b"->"), 300);

        parser.add_prefix(
            ByteStr::from(b"&"), 200);
        parser.add_prefix(
            ByteStr::from(b"@"), 200);

        parser.add_postfix(
            ByteStr::from(b"++"), 190);
        parser.add_postfix(
            ByteStr::from(b"--"), 190);

        // Multiplicative
        parser.add_infixl(
            ByteStr::from(b"*"), 150);
        parser.add_infixl(
            ByteStr::from(b"/"), 150);

        // Additive
        parser.add_infixl(
            ByteStr::from(b"+"), 140);
        parser.add_infixl(
            ByteStr::from(b"-"), 140);

        // Logical
        // TODO(michael): Traditionally these are non-associative. Maybe add that as an option?
        parser.add_infixl(
            ByteStr::from(b"&&"), 120);
        parser.add_infixl(
            ByteStr::from(b"||"), 110);

        // TODO(michael): These are assignment operators
        parser.add_infixl(
            ByteStr::from(b"::"), 50);
        parser.add_infixl(
            ByteStr::from(b":"), 50);
        parser.add_infixl(
            ByteStr::from(b"="), 50);

        parser
    }

    fn add_infixl(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::LeftAssoc{
            op: op,
            lhs: None,
            rhs: None,
        });
    }
    fn add_infixr(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::RightAssoc{
            op: op,
            lhs: None,
            rhs: None,
        });
    }
    fn add_prefix(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::Prefix{
            op: op,
            exp: None,
        });
    }
    fn add_postfix(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::Postfix{
            op: op,
            exp: None,
        });
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

    pub fn parse(&mut self) -> AumResult<Box<Expr>> { self.parse_op() }

    fn parse_op(&mut self) -> AumResult<Box<Expr>> {
        let mut tree: Option<OpTreeNode> = None;

        loop {
            let mut append = |ot: OpTreeNode| {
                if let Some(ref mut t) = tree {
                    t.append(Box::new(ot))
                } else {
                    tree = Some(ot);
                    Ok(())
                }
            };

            match self.peek() {
                Some(Token::Op(_)) => {
                    if let Some(Token::Op(ref bs)) = self.next() {
                        // TODO(michael): Actually implement
                        let treenode = self.operators.get(bs);
                        if let Some(tn) = treenode {
                            try!(append(*tn));
                        } else {
                            return aum_err!("Unrecognized operator: {:?}", bs);
                        }
                    } else {
                        unreachable!()
                    }
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
                Some(Token::Semi) |      // ;
                Some(Token::RParen) |    // )
                Some(Token::RBrace) |    // ]
                Some(Token::RBracket) |  // }
                Some(Token::LBracket) |  // {
                None => break,

                Some(_) => {
                    try!(append(OpTreeNode::Expr{ val: try!(self.parse_atom()) }));
                }
            }
        }

        println!("RESULT: {:?}", tree);

        unimplemented!()
    }

    /// Atoms are any non-operator items!
    fn parse_atom(&mut self) -> AumResult<Box<Expr>> {
        unimplemented!()
    }
}
