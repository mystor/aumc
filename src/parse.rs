use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::fmt;
use std::iter::Peekable;
use std::mem::swap;

use err::*;
use common::{ByteStr, Loc, PrettyPrint};
use lex::{Token, Span};
use ast::*;

#[derive(Debug, Clone)]
enum OpType {
    Op{op: ByteStr},
    Call{args: Vec<Box<Expr>>},
    Index{args: Vec<Box<Expr>>},
}
impl Display for OpType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OpType::Op{ref op} => write!(f, "{}", op),
            OpType::Call{ref args} => write!(f, "({:?})", args),
            OpType::Index{ref args} => write!(f, "[{:?}]", args),
        }
    }
}

/// This data structure is used internally by the parse_op method during
/// the parser's run to the true AST based on operator precidences.
#[derive(Debug, Clone)]
enum OpTreeNode { // TODO(michael): Not a super efficient data structure
    Prefix {
        op: OpType,
        prec: i32,
        exp: Option<Box<OpTreeNode>>,
    },
    Postfix {
        op: OpType,
        prec: i32,
        exp: Option<Box<OpTreeNode>>,
    },
    LeftAssoc {
        op: OpType,
        prec: i32,
        lhs: Option<Box<OpTreeNode>>,
        rhs: Option<Box<OpTreeNode>>,
    },
    RightAssoc {
        op: OpType,
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

                    // Ensure that the connection is valid
                    match *new {
                        Prefix{..} => return aum_err!("Unexpected prefix operator"),
                        Expr{..} => return aum_err!("Unexpected expression"),
                        _ => {}
                    }

                    swap(self, &mut *new);
                    match *self {
                        Prefix{..} => unreachable!(),
                        Expr{..} => unreachable!(),
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

                    // Ensure that the connection is valid
                    match *new {
                        Prefix{..} => return aum_err!("Unexpected prefix operator"),
                        Expr{..} => return aum_err!("Unexpected expression"),
                        _ => {}
                    }

                    swap(self, &mut *new);
                    match *self {
                        Prefix{..} => unreachable!(),
                        Expr{..} => unreachable!(),
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

    /// Transform a OpTreeNode structure into an expression
    fn into_expr(self) -> AumResult<Box<Expr>> {
        Ok(match self {
            OpTreeNode::LeftAssoc{lhs, op: OpType::Op{op}, rhs, ..} |
            OpTreeNode::RightAssoc{lhs, op: OpType::Op{op}, rhs, ..} => Box::new(BinaryOp{
                lhs: try!(lhs.unwrap().into_expr()),
                op: op,
                rhs: try!(rhs.unwrap().into_expr()), // TODO(michael): This unwrap may not succeed, and should be handled
            }),
            OpTreeNode::Prefix{op: OpType::Op{op}, exp, ..} |
            OpTreeNode::Postfix{exp, op: OpType::Op{op}, ..} => Box::new(UnaryOp{
                exp: try!(exp.unwrap().into_expr()), // TODO(michael): This unwrap may not succeed, and should be handled
                op: op,
            }),
            OpTreeNode::Expr{val} => val,

            // Function calls
            OpTreeNode::Postfix{exp, op: OpType::Call{args}, ..} =>
                Box::new(Call{
                    callee: try!(exp.unwrap().into_expr()),
                    args: args
                }),

            // Function calls
            OpTreeNode::Postfix{exp, op: OpType::Index{args}, ..} =>
                Box::new(Index{
                    object: try!(exp.unwrap().into_expr()),
                    args: args
                }),
            _ => unimplemented!(),
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
            op: OpType::Op{op: op},
            prec: prec,
            lhs: None,
            rhs: None,
        });
    }
    fn add_infixr(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::RightAssoc{
            op: OpType::Op{op: op},
            prec: prec,
            lhs: None,
            rhs: None,
        });
    }
    fn add_prefix(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::Prefix{
            op: OpType::Op{op: op},
            prec: prec,
            exp: None,
        });
    }
    fn add_postfix(&mut self, op: ByteStr, prec: i32) {
        let key = op.clone();
        self.operators.insert(key, OpTreeNode::Postfix{
            op: OpType::Op{op: op},
            prec: prec,
            exp: None,
        });
    }

    /// Peek at the next token in the input stream. Returns only the token (if it is present).
    fn peek(&mut self) -> Option<Token> { // TODO(michael): Should this return an Option<&Token>?
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

    /// A block is a series of expressions seperated by semicolons
    pub fn parse_block(&mut self) -> AumResult<Vec<Box<Expr>>> {
        // TODO(michael): Also allow newline-seperated expressions - that would be nice
        let mut exprs = Vec::new();

        loop {
            match self.peek() {
                Some(Token::RParen) | None => break,
                Some(Token::Semi) => { self.next(); continue },
                Some(_) => {
                    exprs.push(try!(self.parse()));
                    if self.peek() != Some(Token::Semi) { break }
                }
            }
        }

        Ok(exprs)
    }

    /// Parse a single expression
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

                // TODO(michael): Get a real logging library to trace this
                println!("PARTIAL: {}", tree.pprint());
                out
            };

            match self.peek() {
                Some(Token::Op(ref bs)) => {
                    self.next();
                    // Look up the operator in the operators table
                    let treenode = self.operators.get(bs);
                    if let Some(tn) = treenode {
                        try!(append(tn.clone()));
                    } else {
                        return aum_err!("Unrecognized operator: {:?}", bs);
                    }
                }

                Some(Token::Ident(id)) => {
                    self.next();
                    try!(append(OpTreeNode::Expr{ val: Box::new(Ident{ name: id }) }));
                }
                Some(Token::Number(num)) => {
                    self.next();
                    try!(append(OpTreeNode::Expr{ val: Box::new(Number{ val: num }) }));
                }

                Some(Token::LBrace) => { // [subscript]
                    self.next();

                    // Get a list of expressions - the actual type of the expression is ambiguous until that point
                    let mut exp_list = Vec::new();

                    loop {
                        if Some(Token::RParen) == self.peek() { self.next(); break }
                        exp_list.push(try!(self.parse()));
                        match self.next() {
                            Some(Token::RBrace) => break,
                            Some(Token::Comma) => continue,
                            Some(x) => return aum_err!("Expected , or ] - instead saw {:?}", x),
                            None => return aum_err!("Unexpected EOF while parsing expression list []"),
                        }
                    }

                    // Check if it is possible to add it as a function call
                    if let Err(_) = append(OpTreeNode::Postfix{
                        op: OpType::Index{args: exp_list.clone()},
                        prec: 250,
                        exp: None,
                    }) {
                        assert!(exp_list.len() == 1); // TODO(michael): Support arbitrary tuples

                        try!(append(OpTreeNode::Expr { val: exp_list[0].clone() })) // TODO(michael): See if we can avoid this copy
                    }
                }

                Some(Token::LParen) => { // (fn call)
                    self.next();

                    // Get a list of expressions - the actual type of the expression is ambiguous until that point
                    let mut exp_list = Vec::new();

                    loop {
                        if Some(Token::RParen) == self.peek() { self.next(); break }
                        exp_list.push(try!(self.parse()));
                        match self.next() {
                            Some(Token::RParen) => break,
                            Some(Token::Comma) => continue,
                            Some(x) => return aum_err!("Expected , or ) - instead saw {:?}", x),
                            None => return aum_err!("Unexpected EOF while parsing expression list ()"),
                        }
                    }

                    if let Some(Token::Op(ref o)) = self.peek() {
                        if &**o == b":" { // TODO(michael): This is nasty
                            // We're looking at a function declaration, and this is the return type
                            self.next();
                            let return_ty = try!(self.parse());

                            if Some(Token::LBracket) != self.next() { return aum_err!("Expected procedure body. Instead found ????"); }

                            let body = try!(self.parse_block());

                            if Some(Token::RBracket) != self.next() { return aum_err!("Expected }} instead found ???"); }

                            try!(append(OpTreeNode::Expr{
                                val: Box::new(Procedure {
                                    params: exp_list,
                                    return_ty: return_ty,
                                    body: body,
                                })
                            }));

                            continue
                        }
                    }

                    if Some(Token::LBracket) == self.peek() {
                        // TODO(michael): Cutnpaste
                        let body = try!(self.parse_block());

                        if Some(Token::RBracket) != self.next() { return aum_err!("Expected }} instead found ???"); }

                        try!(append(OpTreeNode::Expr{
                            val: Box::new(Procedure {
                                params: exp_list,
                                return_ty: Box::new(VoidType) as Box<Expr>,
                                body: body,
                            })
                        }));

                        continue
                    }

                    // Check if it is possible to add it as a function call
                    if let Err(_) = append(OpTreeNode::Postfix{
                        op: OpType::Call{args: exp_list.clone()},
                        prec: 250,
                        exp: None,
                    }) {
                        assert!(exp_list.len() == 1); // TODO(michael): Support arbitrary tuples

                        try!(append(OpTreeNode::Expr { val: exp_list[0].clone() })) // TODO(michael): See if we can avoid this copy
                    }
                }

                // These are valid end-of-expression characters
                Some(Token::Comma) |     // ,
                Some(Token::Semi) |      // ;
                Some(Token::RParen) |    // )
                Some(Token::RBrace) |    // ]
                Some(Token::RBracket) |  // }
                Some(Token::LBracket) |  // {
                None => break,

                Some(a) => return aum_err!("Unexpected token {:?} in input stream", a)
            }
        }

        match tree {
            Some(otn) => otn.into_expr(),
            None => aum_err!("Expected expression, instead got token {:?}", self.peek())
        }
    }
}
