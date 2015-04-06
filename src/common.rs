use std::fmt;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ByteStr(pub Vec<u8>);
impl ByteStr {
    pub fn bytes(&self) -> &[u8] {
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
