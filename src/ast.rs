use std::fmt::Debug;

use err::*;
use common::{ByteStr, Loc, PrettyPrint};

pub trait Expr: Debug + PrettyPrint {
    fn clone_expr(&self) -> Box<Expr>;
    fn validate(&self) -> AumResult<()>;
}
impl Clone for Box<Expr> { // Allow cloning of boxed expressions
    fn clone(&self) -> Box<Expr> {
        self.clone_expr()
    }
}

// AST nodes
#[derive(Debug, Clone)]
pub struct Ident {
    pub name: ByteStr,
}
impl PrettyPrint for Ident {
    fn pprint(&self) -> String {
        format!("{}", self.name)
    }
}
impl Expr for Ident {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub val: ByteStr, // TODO(michael): Should this be a ByteStr?
}
impl PrettyPrint for Number {
    fn pprint(&self) -> String {
        format!("{}", self.val)
    }
}
impl Expr for Number {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub op: ByteStr,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}
impl PrettyPrint for BinaryOp {
    fn pprint(&self) -> String {
        format!("({} {} {})", self.lhs.pprint(), self.op, self.rhs.pprint())
    }
}
impl Expr for BinaryOp {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        try!(self.lhs.validate());
        try!(self.rhs.validate());
        Ok(()) // TODO(michael): Special operators!
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub op: ByteStr,
    pub exp: Box<Expr>,
}
impl PrettyPrint for UnaryOp {
    fn pprint(&self) -> String {
        format!("({} {})", self.op, self.exp.pprint())
    }
}
impl Expr for UnaryOp {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        self.exp.validate()
    }
}

#[derive(Debug, Clone)]
pub struct VoidType;
impl PrettyPrint for VoidType {
    fn pprint(&self) -> String {
        format!("void")
    }
}
impl Expr for VoidType {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Procedure {
    pub params: Vec<Box<Expr>>,
    pub return_ty: Box<Expr>,
    pub body: Vec<Box<Expr>>,
}
impl PrettyPrint for Procedure {
    fn pprint(&self) -> String {
        format!("PLACEHOLDER") // TODO(michael): Complete
    }
}
impl Expr for Procedure {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        for param in &self.params { try!(param.validate()) }
        try!(self.return_ty.validate());
        for stmt in &self.body { try!(stmt.validate()) }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}
impl PrettyPrint for Call {
    fn pprint(&self) -> String {
        // let mut s = String::new();
        // let s = self.args.iter().map(|x| x.pprint()).connect(", ");
        let s: Vec<String> = self.args.iter().map(|x| x.pprint()).collect();
        // for arg in &self.args { s.push_str(&arg.pprint()); s.push_str(","); }
        format!("{}({})", self.callee.pprint(), s.connect(", ")) // TODO(michael): Complete
    }
}
impl Expr for Call {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        try!(self.callee.validate());
        for arg in &self.args { try!(arg.validate()) }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Index {
    pub object: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}
impl PrettyPrint for Index {
    fn pprint(&self) -> String {
        // let mut s = String::new();
        let s: Vec<String> = self.args.iter().map(|x| x.pprint()).collect();
        // for arg in &self.args { s.push_str(&arg.pprint()); s.push_str(","); }
        format!("{}[{}]", self.object.pprint(), s.connect(", ")) // TODO(michael): Complete
    }
}
impl Expr for Index {
    fn clone_expr(&self) -> Box<Expr> { Box::new(self.clone()) }
    fn validate(&self) -> AumResult<()> {
        try!(self.object.validate());
        for arg in &self.args { try!(arg.validate()) }
        Ok(())
    }
}
