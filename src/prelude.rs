pub use crate::{
    fmt::format::{param, params},
    formattable::*,
};

#[macro_export]
macro_rules! _rformat {
    ($format_str:expr,$($params:expr),* $(,)?) => (
        $crate::fmt::format::format_string($format_str, &$crate::fmt::format::params![$($params),*])
    );
}

pub use _rformat as rformat;
