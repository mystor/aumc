use arena::Arena;
use std::cell::RefCell;
use std::collections::HashMap;

use common::ByteStr;
// This file contains the constructs which describe a program

pub struct Program<'a> {
    // The global object.
    global: &'a RefCell<Global<'a>>,
    arena: Arena<'a>,
}
impl <'a> Program<'a> {
    // A wrapper around the arena's alloc method which generates a shared RefCell
    fn alloc<T, F>(&self, op: F) -> &RefCell<T> where F: FnOnce() -> T {
        self.arena.alloc(|| RefCell::new(op()))
    }
}

/// A namespace contains functions and struct/enum declarations
/// These pieces of data are stored within the namespace
pub trait Namespace<'a> {
    fn lookup_item(&self, name: &[u8]) -> Option<&'a RefCell<Type>>;
}
/// A scope contains local automatic variable declarations
pub trait Scope<'a> {
    fn lookup_var_type(&self, name: &[u8]) -> Option<&Option<&'a RefCell<Type>>>;
}

/// A scoped block namespace
pub struct Block<'a> {
    /// The items in the scope
    up_ns: Option<&'a RefCell<Namespace<'a>>>,
    items: HashMap<Vec<u8>, &'a RefCell<Item>>,

    /// Statements
    stmts: Vec<Stmt>,

    /// Declarations
    up_scope: Option<&'a RefCell<Scope<'a>>>,
    decls: HashMap<Vec<u8>, Option<&'a RefCell<Type>>>
}
impl <'a> Namespace<'a> for Block<'a> {
    fn lookup_item(&self, name: &[u8]) -> Option<&'a RefCell<Item>> {
        self.items.get(name).or_else(
            || self.up_ns.map_or(None, |rc| rc.borrow().lookup_item(name)))
    }
}
impl <'a> Scope<'a> for Block<'a> {
    fn lookup_var_type(&self, name: &[u8]) -> Option<&Option<&'a RefCell<Type>>> {
        self.decls.get(name).or_else(
            || self.up_scope.map_or(None, |rc| rc.borrow().lookup_var_type(name)))
    }
}

/// The Global namespace
pub struct Global<'a> {
    items: HashMap<Vec<u8>, &'a RefCell<Item>>,
}
impl <'a> Namespace<'a> for Global<'a> {
    fn lookup_item(&self, name: &[u8]) -> Option<&'a RefCell<Item>> {
        self.items.get(name)
    }
}

/// The scope containing function arguments
pub struct Function<'a> {
    /// Parameters
    params: HashMap<Vec<u8>, Option<&'a RefCell<Type>>>,
    /// Body Block
    body: &'a RefCell<Block<'a>>
}
impl <'a> Scope<'a> for Function<'a> {
    fn lookup_var_type(&self, name: &[u8]) -> Option<&Option<&'a RefCell<Type>>> {
        self.params.get(name)
    }
}

// Items (live in namespaces)
pub struct FunctionItem {
    name: ByteStr
}
impl FunctionItem {
    pub fn as_item(self) -> Item { Item::Function(self) }
}

pub struct EnumItem {
    name: ByteStr
}
impl EnumItem {
    pub fn as_item(self) -> Item { Item::Enum(self) }
}


pub enum Item {
    Function(FunctionItem),
    Enum(EnumItem),
    // TODO(michael): Add constant strings/ints etc as items
}

pub enum Stmt {
    Expr(Expr),
}

pub enum Expr {
    StringLiteral(Vec<u8>),
    NumberLiteral(Vec<u8>),
    BooleanLiteral(bool),
}

pub enum Type {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
}
