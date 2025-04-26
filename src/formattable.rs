#![allow(unused)]

use paste::paste;
use std::fmt;

/// A wrapper type used for the autoref-specialization pattern in formatting traits.
///
/// This wrapper addresses an ambiguity problem with formatting traits:
/// - For any type `T`, its reference `&T` automatically implements certain formatting traits like `fmt::Pointer`.
/// - This makes it challenging to determine if the original type `T` itself implements a specific formatting trait.
///
/// By wrapping `T` in `Formattable<T>`, we can:
/// 1. Implement formatting traits specifically for the wrapped type.
/// 2. Use references to the wrapper (`&Formattable<T>`) for specialized trait detection.
/// 3. Accurately determine which formatting traits the original type implements.
///
/// # Example
///
/// ```rust
/// use rformat::prelude::*;
///
/// let value = 42;
/// let wrapped = &Formattable(&value);
///
/// // Check if the wrapped value implements a specific formatting trait
/// assert!(wrapped.as_debug().is_some());
/// assert!(wrapped.as_pointer().is_none());
/// ```
pub struct Formattable<'t, T>(pub &'t T);

/// This macro generates the necessary traits and implementations to enable the autoref-specialization
/// pattern for determining if a type `T` implements a specific formatting trait (e.g., `std::fmt::Binary`).
///
/// The generated code provides runtime detection of whether a type implements a given formatting trait.
/// If the type implements the trait, the corresponding method (e.g., `as_binary`) will return `Some(...)`,
/// otherwise it will return `None`.
///
/// # Example
///
/// ```
/// # use rformat::prelude::*;
///
/// let a: u32 = 5;
///
/// // `u32` implements `std::fmt::Binary`
/// assert!((&Formattable(&a)).as_binary().is_some());
///
/// // `u32` does not implement `std::fmt::Pointer`
/// assert!((&Formattable(&a)).as_pointer().is_none());
///
/// // `&u32` implements `std::fmt::Pointer`
/// assert!((&Formattable(&&a)).as_pointer().is_some());
/// ```
///
/// # Generated Code
///
/// For a given formatting trait (e.g., `Binary`), the macro generates:
/// - A trait (e.g., `MaybeBinary`) with a method (e.g., `as_binary`) for runtime detection
macro_rules! generate_fmt_option {
    ($format_type:ident) => {
        paste! {
            generate_fmt_option! {$format_type, [<Maybe $format_type>],[<as_ $format_type:snake>]}
        }
    };
    ($format_type:ident, $maybe_format_type:ident, $as_format_type:ident) => {
        /// A trait to check if a type implements the `std::fmt::$format_type` trait.
        pub trait $maybe_format_type {
            /// Returns `Some` if the type implements `std::fmt::$format_type`, otherwise `None`.
            #[inline]
            fn $as_format_type(&self) -> Option<&dyn ::std::fmt::$format_type>;
        }

        impl<T: std::fmt::$format_type> $maybe_format_type for Formattable<'_, T> {
            #[inline]
            fn $as_format_type(&self) -> Option<&dyn ::std::fmt::$format_type> {
                Some(self.0)
            }
        }

        impl<T> $maybe_format_type for &Formattable<'_, T> {
            #[inline]
            fn $as_format_type(&self) -> Option<&dyn ::std::fmt::$format_type> {
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

#[macro_export]
/// A macro to convert a given identifier into a reference to a `Formattable` instance.
///
/// # Parameters
/// - `$formattable`: The identifier to be wrapped in a `Formattable` reference.
///
/// # Example
/// ```rust
/// # use rformat::prelude::*;
/// let my_string = "example";
/// let formattable = to_formattable!(my_string);
/// ```
macro_rules! to_formattable {
    ($formattable:ident) => {
        (&Formattable(&$formattable))
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
        let arg = to_formattable!(val);

        assert!(arg.as_binary().is_some());

        assert!(arg.as_debug().is_none());
        assert!(arg.as_display().is_none());
        assert!(arg.as_lower_exp().is_none());
        assert!(arg.as_lower_hex().is_none());
        assert!(arg.as_octal().is_none());
        assert!(arg.as_pointer().is_none());
        assert!(arg.as_upper_exp().is_none());
        assert!(arg.as_upper_hex().is_none());
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
        let arg = to_formattable!(val);

        assert!(arg.as_debug().is_some());

        assert!(arg.as_binary().is_none());
        assert!(arg.as_display().is_none());
        assert!(arg.as_lower_exp().is_none());
        assert!(arg.as_lower_hex().is_none());
        assert!(arg.as_octal().is_none());
        assert!(arg.as_pointer().is_none());
        assert!(arg.as_upper_exp().is_none());
        assert!(arg.as_upper_hex().is_none());
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
        let arg = to_formattable!(val);

        assert!(arg.as_pointer().is_some());

        assert!(arg.as_binary().is_none());
        assert!(arg.as_debug().is_none());
        assert!(arg.as_display().is_none());
        assert!(arg.as_lower_exp().is_none());
        assert!(arg.as_lower_hex().is_none());
        assert!(arg.as_octal().is_none());
        assert!(arg.as_upper_exp().is_none());
        assert!(arg.as_upper_hex().is_none());
    }
}
