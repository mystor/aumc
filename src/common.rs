use std::fmt;
use std::iter::IntoIterator;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ByteStr(pub Vec<u8>);
impl ByteStr {
    pub fn from<'a, T: IntoIterator<Item = &'a u8>>(slice: T) -> ByteStr {
        ByteStr(slice.into_iter().cloned().collect())
    }
}

impl ::std::ops::Deref for ByteStr {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        let ByteStr(ref bytes) = *self;
        &bytes[..]
    }
}

impl fmt::Debug for ByteStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ByteStr(ref bytes) = *self;
        write!(f, "{:?}", String::from_utf8_lossy(bytes))
    }
}

impl fmt::Display for ByteStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ByteStr(ref bytes) = *self;
        write!(f, "{}", String::from_utf8_lossy(bytes))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}
impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {}", self.line, self.col)
    }
}

pub trait PrettyPrint {
    fn pprint(&self) -> String;
}

impl <T: PrettyPrint> PrettyPrint for Box<T> {
    fn pprint(&self) -> String {
        (**self).pprint()
    }
}

impl <T: PrettyPrint> PrettyPrint for Option<T> {
    fn pprint(&self) -> String {
        match *self {
            Some(ref x) => format!("{}", x.pprint()),
            None => format!("None"),
        }
    }
}
