use arena::GArena;
use std::cell::RefCell;
use std::collections::HashMap;

use common::ByteStr;
// This file contains the constructs which describe a program

pub struct Program<'a> {
    // The global object.
    global: &'a RefCell<Global<'a>>,

    arena: GArena,
}
impl <'a> Program<'a> {
    // Safe wrappers around the arena's alloc method
    fn alloc_block<Ns, Sc>(&self, x: Block<'a, Ns, Sc>) -> &RefCell<Block<'a, Ns, Sc>>
        where Ns: Namespace<'a>, Sc: Scope<'a> {
        unsafe { self.arena.alloc(RefCell::new(x)) }
    }

    fn alloc_function(&self, x: Function<'a>) -> &RefCell<Function<'a>> {
        unsafe { self.arena.alloc(RefCell::new(x)) }
    }

    fn alloc_global(&self, x: Global<'a>) -> &RefCell<Global<'a>> {
        unsafe { self.arena.alloc(RefCell::new(x)) }
    }
}

/// A namespace contains functions and struct/enum declarations
/// These pieces of data are stored within the namespace
pub trait Namespace<'a> {
    fn lookup_item(&self, name: &[u8]) -> Option<&'a RefCell<Item>>;
}
/// A scope contains local automatic variable declarations
pub trait Scope<'a> {
    fn lookup_var_type(&self, name: &[u8]) -> Option<Option<&'a RefCell<Type>>>;
}

/// A scoped block namespace
pub struct Block<'a, Ns: Namespace<'a> + 'a, Sc: Scope<'a> + 'a> {
    /// The items in the scope
    up_ns: Option<&'a RefCell<Ns>>,
    items: HashMap<Vec<u8>, &'a RefCell<Item>>,

    /// Statements
    stmts: Vec<Stmt>,

    /// Declarations
    up_scope: Option<&'a RefCell<Sc>>,
    decls: HashMap<Vec<u8>, Option<&'a RefCell<Type>>>
}
impl <'a, Ns: Namespace<'a>, Sc: Scope<'a>> Namespace<'a> for Block<'a, Ns, Sc> {
    fn lookup_item(&self, name: &[u8]) -> Option<&'a RefCell<Item>> {
        self.items.get(name).map_or_else(
            || self.up_ns.map_or(None, |rc| rc.borrow().lookup_item(name)),
            |x| Some(*x))
    }
}
impl <'a, Ns: Namespace<'a>, Sc: Scope<'a>> Scope<'a> for Block<'a, Ns, Sc> {
    fn lookup_var_type(&self, name: &[u8]) -> Option<Option<&'a RefCell<Type>>> {
        if let Some(&t) = self.decls.get(name) {
            Some(t)
        } else {
            if let Some(rc) = self.up_scope { // Recurse on up_scope
                rc.borrow().lookup_var_type(name)
            } else { // No up scope
                None
            }
        }
    }
}

/// The Global namespace
pub struct Global<'a> {
    items: HashMap<Vec<u8>, &'a RefCell<Item>>,
}
impl <'a> Namespace<'a> for Global<'a> {
    fn lookup_item(&self, name: &[u8]) -> Option<&'a RefCell<Item>> {
        self.items.get(name).map(|x| *x)
    }
}

/// The scope containing function arguments
pub struct Function<'a> {
    /// Parameters
    params: HashMap<Vec<u8>, Option<&'a RefCell<Type>>>,
    /// Body Block
    body: &'a RefCell<Block<'a, Global<'a>, Function<'a>>>
}
impl <'a> Scope<'a> for Function<'a> {
    fn lookup_var_type(&self, name: &[u8]) -> Option<Option<&'a RefCell<Type>>> {
        self.params.get(name).cloned()
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
