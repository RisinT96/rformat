#![allow(unused)]

use trait_mux::trait_mux;

use std::fmt::{Binary, Debug, Display, LowerExp, LowerHex, Octal, Pointer, UpperExp, UpperHex};

pub trait Usize {
    fn as_usize(&self) -> usize;
}

impl Usize for usize {
    fn as_usize(&self) -> usize {
        *self
    }
}

trait_mux!(Formattable {
    Binary,
    Debug,
    Display,
    LowerExp,
    LowerHex,
    Octal,
    Pointer,
    UpperExp,
    UpperHex,
    Usize,
});

pub use into_formattable;
