#![allow(unused)]

use trait_mux::trait_mux;

use std::fmt::{Binary, Debug, Display, LowerExp, LowerHex, Octal, Pointer, UpperExp, UpperHex};

trait_mux!(Formattable {
    Binary,
    Debug,
    Display,
    LowerExp,
    LowerHex,
    Octal,
    Pointer,
    UpperExp,
    UpperHex
});

pub use into_formattable;