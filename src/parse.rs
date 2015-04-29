use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Peekable;
use std::mem::swap;

use err::*;
use common::{ByteStr, Loc, PrettyPrint};
use lex::{Token, Span};

// AST nodes

pub trait Expr: Debug + PrettyPrint {
    fn clone_expr(&self) -> Box<Expr>;
}

impl Clone for Box<Expr> {
    fn clone(&self) -> Box<Expr> {
        self.clone_expr()
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    name: ByteStr,
}
impl PrettyPrint for Ident {
    fn pprint(&self) -> String {
        format!("{}", self.name)
    }
}
impl Expr for Ident {
    fn clone_expr(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    op: ByteStr,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}
impl PrettyPrint for BinaryOp {
    fn pprint(&self) -> String {
        format!("({} {} {})", self.lhs.pprint(), self.op, self.rhs.pprint())
    }
}
impl Expr for BinaryOp {
    fn clone_expr(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    op: ByteStr,
    exp: Box<Expr>,
}
impl PrettyPrint for UnaryOp {
    fn pprint(&self) -> String {
        format!("({} {})", self.op, self.exp.pprint())
    }
}
impl Expr for UnaryOp {
    fn clone_expr(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
}

/// This data structure is used internally by the parse_op method during
/// the parser's run to the true AST based on operator precidences.
#[derive(Debug, Clone)]
enum OpTreeNode { // TODO(michael): Not a super efficient data structure
    Prefix {
        op: ByteStr,
        prec: i32,
        exp: Option<Box<OpTreeNode>>,
    },
    Postfix {
        op: ByteStr,
        prec: i32,
        exp: Option<Box<OpTreeNode>>,
    },
    LeftAssoc {
        op: ByteStr,
        prec: i32,
        lhs: Option<Box<OpTreeNode>>,
        rhs: Option<Box<OpTreeNode>>,
    },
    RightAssoc {
        op: ByteStr,
        prec: i32,
        lhs: Option<Box<OpTreeNode>>,
        rhs: Option<Box<OpTreeNode>>,
    },
    Expr {
        val: Box<Expr>,
    }
}
impl OpTreeNode {
    fn precidence(&self) -> i32 {
        match *self {
            OpTreeNode::Prefix{prec, ..} => prec,
            OpTreeNode::Postfix{prec, ..} => prec,
            OpTreeNode::LeftAssoc{prec, ..} => prec,
            OpTreeNode::RightAssoc{prec, ..} => prec,
            OpTreeNode::Expr{..} => ::std::i32::MAX,
        }
    }

    fn append(&mut self, mut new: Box<OpTreeNode>) -> AumResult<()> {
        use self::OpTreeNode::*;

        let prec = self.precidence();

        match *self {
            Prefix{exp: ref mut exp @ None, ..} => {
                match *new {
                    Prefix{..} | Expr{..} => *exp = Some(new),
                    _ => return aum_err!("Prefix expression without body"),
                }
            }
            Prefix{exp: Some(_), ..} => {
                // TODO(michael): Implement more correctly
                if new.precidence() > prec {
                    // Get a reference to the child tree node
                    // we couldn't do this before because of the other branch of the if
                    if let Prefix{exp: Some(ref mut tn), ..} = *self {
                        try!(tn.append(new));
                    } else { unreachable!() }
                } else {
                    // TODO(michael): Cutnpaste from Expr{..} | Postfix{..}
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

    fn into_expr(self) -> AumResult<Box<Expr>> {
        Ok(match self {
            OpTreeNode::LeftAssoc{lhs, op, rhs, ..} |
            OpTreeNode::RightAssoc{lhs, op, rhs, ..} => Box::new(BinaryOp{
                lhs: try!(lhs.unwrap().into_expr()),
                op: op,
                rhs: try!(rhs.unwrap().into_expr()), // TODO(michael): This unwrap may not succeed, and should be handled
            }),
            OpTreeNode::Prefix{op, exp, ..} |
            OpTreeNode::Postfix{exp, op, ..} => Box::new(UnaryOp{
                exp: try!(exp.unwrap().into_expr()), // TODO(michael): This unwrap may not succeed, and should be handled
                op: op,
            }),
            OpTreeNode::Expr{val} => val,
        })
    }
}
impl PrettyPrint for OpTreeNode {
    fn pprint(&self) -> String {
        match *self {
            OpTreeNode::LeftAssoc{ref lhs, ref op, ref rhs, ..} => format!("({} {} {})", lhs.pprint(), op, rhs.pprint()),
            OpTreeNode::RightAssoc{ref lhs, ref op, ref rhs, ..} => format!("({} {} {})", lhs.pprint(), op, rhs.pprint()),
            OpTreeNode::Prefix{ref exp, ref op, ..} => format!("({} {})", op, exp.pprint()),
            OpTreeNode::Postfix{ref exp, ref op, ..} => format!("({} {})", exp.pprint(), op),
            OpTreeNode::Expr{ref val} => format!("{}", val.pprint()),
        }
    }
}


/// The actual parser. This object holds a tokens iterator (for example, Lex), and
/// will produce expressions when `parse` is called on it.
/// A program in aum is a list of expressions.
pub struct Parser<T: Iterator<Item = Span>> {
    tokens: Peekable<T>,

    operators: HashMap<ByteStr, OpTreeNode>, // Prototypical OpTreeNodes

    toplevel: bool,
}

impl <T: Iterator<Item = Span>> Parser<T> {
    pub fn new(it: T) -> Parser<T> {
        let mut parser = Parser {
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
        parser.add_infixl(
            ByteStr::from(b":="), 50);

        parser
    }

    fn add_infixl(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::LeftAssoc{
            op: op,
            prec: prec,
            lhs: None,
            rhs: None,
        });
    }
    fn add_infixr(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::RightAssoc{
            op: op,
            prec: prec,
            lhs: None,
            rhs: None,
        });
    }
    fn add_prefix(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::Prefix{
            op: op,
            prec: prec,
            exp: None,
        });
    }
    fn add_postfix(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::Postfix{
            op: op,
            prec: prec,
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
                let out = if let Some(ref mut t) = tree {
                    t.append(Box::new(ot))
                } else {
                    tree = Some(ot);
                    Ok(())
                };

                println!("PARTIAL: {}", tree.pprint());
                out
            };

            match self.peek() {
                Some(Token::Op(_)) => {
                    if let Some(Token::Op(ref bs)) = self.next() {
                        // TODO(michael): Actually implement
                        let treenode = self.operators.get(bs);
                        if let Some(tn) = treenode {
                            try!(append(tn.clone()));
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
                // Some(Token::Colon) |     // :
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

        println!("RESULT: {}", tree.pprint());

        match tree {
            Some(otn) => otn.into_expr(),
            None => aum_err!("Expected expression, instead got token {:?}", self.peek())
        }
    }

    /// Atoms are any non-operator items!
    fn parse_atom(&mut self) -> AumResult<Box<Expr>> {
        Ok(match self.next() {
            Some(Token::Ident(x)) => Box::new(Ident{ name: x.clone() }),

            Some(tok) => return aum_err!("Unrecognized token in input stream: {:?}", tok),
            None => return aum_err!("Unexpected EOF while parsing expression"),
        })
    }
}
