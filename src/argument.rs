#![allow(unused)]

use paste::paste;
use std::fmt;

/// A wrapper type used for autoref-specialization pattern in formatting traits.
///
/// This wrapper solves an ambiguity problem with formatting traits:
/// - For any type `T`, its reference `&T` automatically implements `fmt::Pointer`
/// - This makes it difficult to determine if the original type `T` itself implements a format trait
///
/// By wrapping `T` in `Wrap<T>`, we can:
/// 1. Implement formatting traits specifically for the wrapped type
/// 2. Use references to the wrapper (`&Wrap<T>`) for specialized trait detection
/// 3. Correctly determine which formatting traits the original type implements
pub struct Wrap<'a, T>(pub &'a T);

/// This macro generates the necessary structs and traits to use the autoref-specialization pattern
/// for determining if a type `T` implements a specific formatting trait (e.g., `std::fmt::Binary`).
///
/// The generated code allows for runtime detection of whether a type implements a given formatting
/// trait. If the type implements the trait, the `wrap` method will return `Some(...)`, otherwise it
/// will return `None`.
///
/// # Example
///
/// ```ignore
/// generate_fmt_option!(Binary);
/// generate_fmt_option!(Pointer);
/// ```
///
/// # Usage
///
/// ```
/// # use rformat::prelude::*;
///
/// let a: u32 = 5;
///
/// // `u32` implements `std::fmt::Binary`
/// assert!((&Wrap(&a)).binary().wrap(&a).is_some());
///
/// // `u32` does not implement `std::fmt::Pointer`
/// assert!((&Wrap(&a)).pointer().wrap(&a).is_none());
///
/// // `&u32` implements `std::fmt::Pointer`
/// assert!((&Wrap(&&a)).pointer().wrap(&&a).is_some());
/// ```
///
/// # Generated Code
///
/// For a given formatting trait (e.g., `Binary`), the macro generates:
/// - A tag struct (`BinaryTag`) and a "no tag" struct (`NoBinaryTag`)
/// - Two traits: `BinaryKind` and `NoBinaryKind`
/// - Implementations of these traits for `Wrap<T>` and `&Wrap<T>`
/// - A `wrap` method for the tag structs to return `Some` or `None` based on whether the type
///   implements the formatting trait
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

            impl<T: std::fmt::$name> $kind for Wrap<'_, T> {}
            impl<T> $no_kind for &Wrap<'_, T> {}

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

// Generate autoref-specialization support for known formatting traits.
generate_fmt_option!(Binary);
generate_fmt_option!(Debug);
generate_fmt_option!(Display);
generate_fmt_option!(LowerExp);
generate_fmt_option!(LowerHex);
generate_fmt_option!(Octal);
generate_fmt_option!(Pointer);
generate_fmt_option!(UpperExp);
generate_fmt_option!(UpperHex);

/// Represents a collection of optional formatting traits for a value.
///
/// The `Argument` struct holds references to various formatting traits
/// that can be used to format a value in different styles. Each field
/// corresponds to a specific formatting trait, and the field is `None`
/// if the trait is not implemented for the value.
///
/// # Fields
/// - `binary`: Optional reference to a type implementing the `fmt::Binary` trait,
///   used for binary formatting (e.g., `0b1010`).
/// - `debug`: Optional reference to a type implementing the `fmt::Debug` trait,
///   used for developer-facing formatting (e.g., `"{:?}"`).
/// - `display`: Optional reference to a type implementing the `fmt::Display` trait,
///   used for user-facing formatting (e.g., `"{}"`).
/// - `lower_exp`: Optional reference to a type implementing the `fmt::LowerExp` trait,
///   used for scientific notation with lowercase `e` (e.g., `"1.0e10"`).
/// - `lower_hex`: Optional reference to a type implementing the `fmt::LowerHex` trait,
///   used for hexadecimal formatting with lowercase letters (e.g., `"0x1a"`).
/// - `octal`: Optional reference to a type implementing the `fmt::Octal` trait,
///   used for octal formatting (e.g., `"0o12"`).
/// - `pointer`: Optional reference to a type implementing the `fmt::Pointer` trait,
///   used for pointer formatting (e.g., `"0x1234abcd"`).
/// - `upper_exp`: Optional reference to a type implementing the `fmt::UpperExp` trait,
///   used for scientific notation with uppercase `E` (e.g., `"1.0E10"`).
/// - `upper_hex`: Optional reference to a type implementing the `fmt::UpperHex` trait,
///   used for hexadecimal formatting with uppercase letters (e.g., `"0x1A"`).
pub struct Argument<'a> {
    pub name: &'a str,
    pub binary: Option<&'a dyn fmt::Binary>,
    pub debug: Option<&'a dyn fmt::Debug>,
    pub display: Option<&'a dyn fmt::Display>,
    pub lower_exp: Option<&'a dyn fmt::LowerExp>,
    pub lower_hex: Option<&'a dyn fmt::LowerHex>,
    pub octal: Option<&'a dyn fmt::Octal>,
    pub pointer: Option<&'a dyn fmt::Pointer>,
    pub upper_exp: Option<&'a dyn fmt::UpperExp>,
    pub upper_hex: Option<&'a dyn fmt::UpperHex>,
}

#[macro_export]
macro_rules! argument_option {
    ($ty:ident, $value:ident) => {
        (&Wrap(&$value)).$ty().wrap(&$value)
    };
}

/// A macro to create an `Argument` struct by populating its fields using the `argument_option!` macro.
///
/// # Parameters
///
/// - `$value`: The identifier that is passed to the `argument_option!` macro to generate the values
///   for the fields of the `Argument` struct.
///
/// # Example
///
/// ```rust
/// # use rformat::prelude::*;
///
/// let my_value = 5;
/// let argument = to_argument!(my_value);
/// ```
#[macro_export]
macro_rules! to_argument {
    ($value:ident) => {
        Argument {
            name: stringify!($value),
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

#[cfg(test)]
mod tests {
    use crate::prelude::*;
    use std::fmt;

    #[test]
    fn check_binary() {
        struct A;
        impl fmt::Binary for A {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }

        let val = A;
        let arg = to_argument!(val);

        assert_eq!(arg.name, "val");

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
        let arg = to_argument!(val);

        assert_eq!(arg.name, "val");

        assert!(arg.debug.is_some());

        assert!(arg.binary.is_none());
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
        let arg = to_argument!(val);

        assert_eq!(arg.name, "val");

        assert!(arg.pointer.is_some());

        assert!(arg.binary.is_none());
        assert!(arg.debug.is_none());
        assert!(arg.display.is_none());
        assert!(arg.lower_exp.is_none());
        assert!(arg.lower_hex.is_none());
        assert!(arg.octal.is_none());
        assert!(arg.upper_exp.is_none());
        assert!(arg.upper_hex.is_none());
    }
}
