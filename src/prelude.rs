pub use crate::{
    fmt::format::{param, params},
    formattable::*,
};

#[macro_export]
macro_rules! _rformat {
    ($fmt:expr) => {
        $crate::fmt::format::format_string($fmt, &[])
    };
    ($fmt:expr, $($args:tt)*) => {
        $crate::fmt::format::format_string($fmt, &$crate::fmt::format::params![$($args)*])
    };
}

pub use _rformat as rformat;
