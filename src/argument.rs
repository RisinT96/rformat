#![allow(unused)]

use paste::paste;
use std::fmt::{self, write};

/// Type wrapper
/// Workaround to disambiguate [Pointer](std::fmt::Pointer) types.
/// Whatever the type `T`, `&T` will always implement pointer formatting, this causes some ambiguity
/// regarding whether `T` is the type or the reference to the type.
/// Using the wrapper will disambiguate T, while referencing the wrapper allows us to use
/// autoref-specialization
pub struct Wrap<T>(pub T);

/// Generates structs and traits to use autoref-specialization to determine if a type T implements
/// the format trait, depending on that, `wrap` returns `None` or `Some(...)`
/// Example:
/// ```ignore
/// generate_fmt_option!(Binary);
/// generate_fmt_option!(Pointer);
/// ```
/// ```
/// # use rformat::argument::*;
///
/// let a: u32 = 5;
///
/// // u32 implements std::fmt::Binary
/// assert!((&Wrap(a)).binary().wrap(&a).is_some());
/// // u32 does not implement std::fmt::Pointer
/// assert!((&Wrap(a)).pointer().wrap(&a).is_none());
/// // &u32 implements std::fmt::Pointer
/// assert!((&Wrap(&a)).pointer().wrap(&&a).is_some());
/// ```
macro_rules! generate_fmt_option {
    ($name:ident) => {
        paste! {
            generate_fmt_option! {[<$name Tag>], [<No $name Tag>], [<$name Kind>], [<No $name Kind>], $name, [<$name:snake>]}
        }
    };
    ($tag:ident, $no_tag:ident, $kind:ident, $no_kind:ident, $name: ident, $name_snake: ident) => {
            pub struct $tag;
            pub struct $no_tag;

            pub trait $kind {
                #[inline]
                fn $name_snake(&self) -> $tag {
                    $tag
                }
            }

            pub trait $no_kind {
                #[inline]
                fn $name_snake(&self) -> $no_tag {
                    $no_tag
                }
            }

            impl<T: std::fmt::$name> $kind for Wrap<T> {}
            impl<T> $no_kind for &Wrap<T> {}

            impl $tag {
                #[inline]
                pub fn wrap<T: std::fmt::$name>(self, value: &T) -> Option<&dyn std::fmt::$name> {
                    Some(value)
                }
            }
            impl $no_tag {
                #[inline]
                pub fn wrap<T>(self, _: &T) -> Option<&dyn std::fmt::$name> {
                    None
                }
            }
    };
}

generate_fmt_option!(Binary);
generate_fmt_option!(Debug);
generate_fmt_option!(Display);
generate_fmt_option!(LowerExp);
generate_fmt_option!(LowerHex);
generate_fmt_option!(Octal);
generate_fmt_option!(Pointer);
generate_fmt_option!(UpperExp);
generate_fmt_option!(UpperHex);

/// Struct holds references to the same variable, but with different trait impls.
/// This is necessary to pass trait impl information into the runtime format string evaluator.
/// If a format hint is not implemented, the appropriate value will contain a `None`, and will
/// return an error at runtime.
pub struct Argument<'a> {
    binary: Option<&'a dyn fmt::Binary>,
    debug: Option<&'a dyn fmt::Debug>,
    display: Option<&'a dyn fmt::Display>,
    lower_exp: Option<&'a dyn fmt::LowerExp>,
    lower_hex: Option<&'a dyn fmt::LowerHex>,
    octal: Option<&'a dyn fmt::Octal>,
    pointer: Option<&'a dyn fmt::Pointer>,
    upper_exp: Option<&'a dyn fmt::UpperExp>,
    upper_hex: Option<&'a dyn fmt::UpperHex>,
}

#[macro_export]
macro_rules! argument_option {
    ($ty:ident, $value:ident) => {
        (&Wrap($value)).$ty().wrap(&$value)
    };
}

#[macro_export]
macro_rules! argument {
    ($value:ident) => {
        Argument {
            binary: argument_option!(binary, $value),
            debug: argument_option!(debug, $value),
            display: argument_option!(display, $value),
            lower_exp: argument_option!(lower_exp, $value),
            lower_hex: argument_option!(lower_hex, $value),
            octal: argument_option!(octal, $value),
            pointer: argument_option!(pointer, $value),
            upper_exp: argument_option!(upper_exp, $value),
            upper_hex: argument_option!(upper_hex, $value),
        }
    };
}

macro_rules! impl_fmt_option {
    ($name:ident) => {
        paste! {
            impl_fmt_option! {$name, [<$name:snake>]}
        }
    };
    ($name: ident, $name_snake: ident) => {
        impl std::fmt::$name for Argument<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let value = self.$name_snake.ok_or(std::fmt::Error)?;

                std::fmt::$name::fmt(value, f)
            }
        }
    };
}

impl_fmt_option!(Binary);
impl_fmt_option!(Debug);
impl_fmt_option!(Display);
impl_fmt_option!(LowerExp);
impl_fmt_option!(LowerHex);
impl_fmt_option!(Octal);
impl_fmt_option!(Pointer);
impl_fmt_option!(UpperExp);
impl_fmt_option!(UpperHex);

#[cfg(test)]
mod tests {
    use crate::argument::*;

    #[test]
    fn check_binary() {
        struct A;
        impl fmt::Binary for A {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }

        let val = A;

        let arg = argument!(A);

        assert!(arg.binary.is_some());
        assert!(arg.debug.is_none());
        assert!(arg.display.is_none());
        assert!(arg.lower_exp.is_none());
        assert!(arg.lower_hex.is_none());
        assert!(arg.octal.is_none());
        assert!(arg.pointer.is_none());
        assert!(arg.upper_exp.is_none());
        assert!(arg.upper_hex.is_none());
    }

    #[test]
    fn check_debug() {
        struct A;
        impl fmt::Debug for A {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }

        let val = A;

        let arg = argument!(A);

        assert!(arg.binary.is_none());
        assert!(arg.debug.is_some());
        assert!(arg.display.is_none());
        assert!(arg.lower_exp.is_none());
        assert!(arg.lower_hex.is_none());
        assert!(arg.octal.is_none());
        assert!(arg.pointer.is_none());
        assert!(arg.upper_exp.is_none());
        assert!(arg.upper_hex.is_none());
    }

    #[test]
    fn check_pointer() {
        struct A;
        impl fmt::Pointer for A {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }

        let val = A;

        let arg = argument!(A);

        assert!(arg.binary.is_none());
        assert!(arg.debug.is_none());
        assert!(arg.display.is_none());
        assert!(arg.lower_exp.is_none());
        assert!(arg.lower_hex.is_none());
        assert!(arg.octal.is_none());
        assert!(arg.pointer.is_some());
        assert!(arg.upper_exp.is_none());
        assert!(arg.upper_hex.is_none());
    }

    macro_rules! check_fmt {
        ($ty:ident, $hint:literal) => {
            paste! {
                #[test]
                fn [<check_fmt_ $ty>]() {
                    let a = 12345;
                    let arg = argument!(a);

                    assert_eq!(format!(concat!("{:",$hint,"}"),a),
                               format!(concat!("{:",$hint,"}"),arg)
                    );
                }
            }
        };
    }

    check_fmt!(binary, "b");
    check_fmt!(debug, "?");
    check_fmt!(display, "");
    check_fmt!(lower_exp, "e");
    check_fmt!(lower_hex, "x");
    check_fmt!(octal, "o");
    check_fmt!(upper_exp, "E");
    check_fmt!(upper_hex, "X");

    #[test]
    fn check_fmt_pointer() {
        let a = 12345;
        let ref_a = &a;
        let arg = argument!(ref_a);

        assert_eq!(format!("{:p}", ref_a), format!("{:p}", arg));
    }
}
