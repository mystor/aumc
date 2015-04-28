pub type AumResult<T> = Result<T, AumErr>;

pub struct AumErr {
    pub message: String,
}

#[macro_export]
macro_rules! aum_err {
    ($($x:expr),+) => { // Expands to an aumerr object -- at some point, I should add more stuff
        Err(AumErr{
            message: format!($($x),+)
        })
    }
}

#[macro_export]
macro_rules! aum_try {
    ($ctx:expr, $expr:expr) => {
        match $expr {
            Ok(x) => x,
            Err(mut e) => {
                e.message.push_str(x, $ctx);
                return Err(e);
            }
        }
    }
}
