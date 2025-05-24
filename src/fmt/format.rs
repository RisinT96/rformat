use crate::error::{FormatError, Result};
use crate::fmt::violence;
use crate::format_spec::{self, Argument, Count, FormatSpec, Precision};
use crate::prelude::*;

/// Represents a formatting parameter with an identifier and its formattable value.
///
/// The identifier is used for named parameter access in format strings.
/// The formattable is the actual value that will be formatted.
pub struct Parameter<'ident, 'formattable> {
    /// The string identifier of the parameter, used for named parameter access.
    pub identifier: &'ident str,
    /// The formattable value that will be formatted.
    pub formattable: Formattable<'formattable>,
}

/// Creates a formatting parameter from an expression.
///
/// This macro has two forms:
/// - `param!(name = expr)` - Creates a parameter with the specified name and value
/// - `param!(expr)` - Creates a parameter with the name derived from the expression
#[macro_export]
macro_rules! _param {
    ($param_name:ident = $param:expr) => {
        $crate::fmt::format::Parameter {
            identifier: ::core::stringify!($param_name),
            formattable: $crate::formattable::into_formattable!($param),
        }
    };
    ($param:expr) => {
        $crate::fmt::format::Parameter {
            identifier: ::core::stringify!($param),
            formattable: $crate::formattable::into_formattable!($param),
        }
    };
}
pub use _param as param;

/// Creates a vector of formatting parameters.
///
/// This macro takes a comma-separated list of expressions and converts each into a `Parameter`.
/// Each expression is processed by the `param!` macro.
#[macro_export]
macro_rules! _params {
    // Empty list
    () => (vec![]);

    // Single named parameter
    ($name:ident = $value:expr) => (
        vec![$crate::fmt::format::param!($name = $value)]
    );

    // Single unnamed parameter
    ($value:expr) => (
        vec![$crate::fmt::format::param!($value)]
    );

    // Only named parameters
    ($($name:ident = $value:expr),*) => (
        vec![$($crate::fmt::format::param!($name = $value)),*]
    );

    // Mixed named and unnamed parameters - we need to use TT munching

    // Stop at empty tail.
    (@acc [$($acc:tt)*];) => (vec![$($acc)*]);

    // Handle last named parameter
    (@acc [$($acc:tt)*]; $name:ident = $value:expr) => (
        vec![$($acc)*, $crate::fmt::format::param!($name = $value)]
    );

    // Handle last unnamed parameter
    (@acc [$($acc:tt)*]; $value:expr) => (
        vec![$($acc)*, $crate::fmt::format::param!($value)]
    );

    // Handle a named parameter
    (@acc [$($acc:tt)*]; $name:ident = $value:expr, $($tail:tt)*) => (
        $crate::fmt::format::params!(@acc [$($acc)*, $crate::fmt::format::param!($name = $value)]; $($tail)*)
    );

    // Handle an unnamed parameter
    (@acc [$($acc:tt)*]; $value:expr, $($tail:tt)*) => (
        $crate::fmt::format::params!(@acc [$($acc)*, $crate::fmt::format::param!($value)]; $($tail)*)
    );

    // Start with a named parameter
    ($name:ident = $value:expr, $($tail:tt)*) => (
        $crate::fmt::format::params!(@acc [$crate::fmt::format::param!($name = $value)]; $($tail)*)
    );

    // Start with an unnamed parameter
    ($value:expr, $($tail:tt)*) => (
        $crate::fmt::format::params!(@acc [$crate::fmt::format::param!($value)]; $($tail)*)
    );
}
pub use _params as params;

/// Formats a string according to the given format specification and parameters.
///
/// This function parses the format string, identifies format placeholders,
/// and replaces them with formatted parameter values.
///
/// # Arguments
///
/// * `fmt` - A string containing format specifications
/// * `params` - A slice of parameters to be formatted
///
/// # Returns
///
/// A `Result` containing the formatted string or an error if formatting fails.
///
/// # Format Syntax
///
/// The format string follows Rust's standard format syntax.
pub fn format_string(fmt: &str, params: &[Parameter]) -> Result<String> {
    let mut result = String::new();

    // Iterator over parameters for sequential access
    let mut param_iter = params.iter();

    // Peekable iterator over characters with their positions
    let mut chars = fmt.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        if c == '{' {
            if let Some((_, '{')) = chars.peek() {
                // Escaped opening brace: `{{` becomes `{`
                chars.next();
                result.push('{');
            } else {
                // Start of a format placeholder
                let mut format = String::new();
                let mut found = false;

                // Collect characters until finding a closing brace
                for (_, next) in chars.by_ref() {
                    if next == '}' {
                        found = true;
                        break;
                    }

                    format.push(next);
                }

                // Error if no closing brace was found
                if !found {
                    return Err(FormatError::UnmatchedOpeningBrace(i, fmt.to_string()));
                }

                format_and_write_value(&mut result, &format, params, &mut param_iter)?;
            }
        } else if c == '}' {
            if let Some((_, '}')) = chars.peek() {
                // Escaped closing brace: `}}` becomes `}`
                chars.next();
                result.push('}');
            } else {
                // Unmatched closing brace is an error
                return Err(FormatError::UnmatchedClosingBrace(i, fmt.to_string()));
            }
        } else {
            // Regular character, just copy to output
            result.push(c);
        }
    }

    Ok(result)
}

/// Internal representation of a parsed format placeholder.
#[derive(Debug, PartialEq)]
struct Format<'format_string> {
    /// The argument specification (identifier, position, or None for next parameter)
    argument: Option<Argument<'format_string>>,
    /// The formatting specification that controls how the value is formatted
    format_spec: FormatSpec<'format_string>,
}

/// Formats a value according to the given format specification and appends it to a writer.
///
/// This function parses the format specification, resolves the parameter to format,
/// and applies the formatting rules.
///
/// # Arguments
///
/// * `dst` - The destination to write the formatted value to
/// * `format` - The format specification string
/// * `app_params` - All available parameters
/// * `params_iter` - An iterator over parameters for sequential access
///
/// # Returns
///
/// A `Result` indicating success or failure
fn format_and_write_value<
    'param,
    'ident,
    'formattable,
    W: std::fmt::Write,
    I: Iterator<Item = &'param Parameter<'ident, 'formattable>>,
>(
    dst: &mut W,
    format: &str,
    app_params: &'param [Parameter<'ident, 'formattable>],
    params_iter: &mut I,
) -> Result<()>
where
    'ident: 'param,
    'formattable: 'param,
{
    let format = parse_format(format)?;
    let format_spec = &format.format_spec;

    // Handle precision first, as precision=* will consume a parameter,
    // affecting the parameter iterator state before we get to the main parameter
    let precision = resolve_precision_value(&format_spec.precision, app_params, params_iter)?;

    // Resolve the parameter to format
    let parameter = get_parameter_for_formatting(&format.argument, app_params, params_iter)?;

    // Resolve width value, defaulting to 0 if not specified
    let width = resolve_width_value(&format_spec, app_params)?.unwrap_or(0);

    // Select the appropriate formatting method based on the format type
    let out = match format_spec.r#type {
        format_spec::Type::Binary => {
            violence::format_binary(format_spec, precision, width, parameter)
        }
        format_spec::Type::Debug => {
            violence::format_debug(format_spec, precision, width, parameter)
        }
        format_spec::Type::DebugLowerHex => {
            violence::format_debug_lower_hex(format_spec, precision, width, parameter)
        }
        format_spec::Type::DebugUpperHex => {
            violence::format_debug_upper_hex(format_spec, precision, width, parameter)
        }
        format_spec::Type::Display => {
            violence::format_display(format_spec, precision, width, parameter)
        }
        format_spec::Type::LowerExp => {
            violence::format_lower_exp(format_spec, precision, width, parameter)
        }
        format_spec::Type::LowerHex => {
            violence::format_lower_hex(format_spec, precision, width, parameter)
        }
        format_spec::Type::Octal => {
            violence::format_octal(format_spec, precision, width, parameter)
        }
        format_spec::Type::Pointer => {
            violence::format_pointer(format_spec, precision, width, parameter)
        }
        format_spec::Type::UpperExp => {
            violence::format_upper_exp(format_spec, precision, width, parameter)
        }
        format_spec::Type::UpperHex => {
            violence::format_upper_hex(format_spec, precision, width, parameter)
        }
    }?;

    // Replace placeholder characters with the actual fill character
    let mut fill = [0; 4];
    let out = out.replace("\u{001A}", format_spec.fill.encode_utf8(&mut fill));

    write!(dst, "{out}")?;

    Ok(())
}

/// Resolves the precision value from the format specification.
///
/// Precision can be:
/// - A direct integer value
/// - An argument reference to a parameter
/// - A '*' that consumes the next parameter
///
/// # Returns
///
/// `Some(usize)` if precision is specified, `None` otherwise
fn resolve_precision_value<
    'param,
    'ident,
    'formattable,
    I: Iterator<Item = &'param Parameter<'ident, 'formattable>>,
>(
    precision: &Option<Precision>,
    params: &'param [Parameter<'ident, 'formattable>],
    params_iter: &mut I,
) -> Result<Option<usize>>
where
    'ident: 'param,
    'formattable: 'param,
{
    if let Some(precision) = precision {
        let precision = match precision {
            Precision::Star => params_iter.next().ok_or(FormatError::NotEnoughParameters)?,
            Precision::Count(Count::Argument(a)) => lookup_parameter_by_argument(a, params)?,
            Precision::Count(Count::Integer(i)) => return Ok(Some(*i)),
        };

        let param = precision
            .formattable
            .try_as_usize()
            .ok_or(FormatError::PrecisionNotUsize)?
            .as_usize();

        Ok(Some(param))
    } else {
        Ok(None)
    }
}

/// Resolves the width value from the format specification.
///
/// Width can be:
/// - A direct integer value
/// - An argument reference to a parameter
///
/// # Returns
///
/// `Some(usize)` if width is specified, `None` otherwise
fn resolve_width_value<'param, 'ident, 'formattable>(
    format_spec: &FormatSpec,
    params: &'param [Parameter<'ident, 'formattable>],
) -> Result<Option<usize>>
where
    'ident: 'param,
    'formattable: 'param,
{
    Ok(match &format_spec.width {
        Some(w) => {
            let param = match w {
                Count::Argument(a) => lookup_parameter_by_argument(a, params)?,
                Count::Integer(i) => return Ok(Some(*i)),
            };

            Some(
                param
                    .formattable
                    .try_as_usize()
                    .ok_or(FormatError::WidthNotUsize)?
                    .as_usize(),
            )
        }
        None => None,
    })
}

/// Gets the parameter to format based on the argument specification.
///
/// If no argument is specified, takes the next parameter from the iterator.
/// If an argument is specified, looks up the parameter by name or position.
fn get_parameter_for_formatting<
    'param,
    'ident,
    'formattable,
    I: Iterator<Item = &'param Parameter<'ident, 'formattable>>,
>(
    argument: &Option<Argument>,
    params: &'param [Parameter<'ident, 'formattable>],
    params_iter: &mut I,
) -> Result<&'param Parameter<'ident, 'formattable>>
where
    'ident: 'param,
    'formattable: 'param,
{
    Ok(match argument {
        None => params_iter.next().ok_or(FormatError::NotEnoughParameters)?,
        Some(a) => lookup_parameter_by_argument(a, params)?,
    })
}

/// Looks up a parameter by argument specification.
///
/// # Arguments
///
/// * `argument` - The argument specification (identifier or position)
/// * `parameters` - The available parameters
///
/// # Returns
///
/// A reference to the matching parameter or an error if not found
fn lookup_parameter_by_argument<'param, 'ident, 'formattable>(
    argument: &Argument,
    parameters: &'param [Parameter<'ident, 'formattable>],
) -> Result<&'param Parameter<'ident, 'formattable>> {
    match argument {
        Argument::Identifier(i) => parameters
            .iter()
            .find(|a| a.identifier == *i)
            .ok_or_else(|| FormatError::NoNamedParameter(i.to_string())),
        Argument::Integer(i) => parameters
            .get(*i)
            .ok_or(FormatError::NoPositionalParameter(*i)),
    }
}

/// Parses a format string into a `Format` struct.
///
/// Format syntax: https://doc.rust-lang.org/std/fmt/#syntax
///
/// # Arguments
///
/// * `format` - The format string content inside braces (without the braces)
///
/// # Returns
///
/// A parsed `Format` struct or an error if the format is invalid
fn parse_format(format: &str) -> Result<Format> {
    // Split into argument and format specification parts at the first colon
    let (argument, format_spec) = format.split_once(':').unwrap_or((format, ""));

    // Parse the argument part
    let argument = if argument.is_empty() {
        None
    } else if argument.chars().all(|c| c.is_ascii_digit()) {
        Some(Argument::Integer(argument.parse::<usize>()?))
    } else {
        Some(Argument::Identifier(argument))
    };

    // Parse the format specification part
    let format_spec = format_spec::parse_format_spec(format_spec)?;

    Ok(Format {
        argument,
        format_spec,
    })
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    macro_rules! assert_format {
        ($fmt:expr) => {
            ::core::assert_eq!(
                ::std::format!($fmt),
                $crate::prelude::rformat!($fmt).unwrap()
            )
        };
        ($fmt:expr, $($args:tt)*) => {
            ::core::assert_eq!(
                ::std::format!($fmt, $($args)*),
                $crate::prelude::rformat!($fmt, $($args)*).unwrap()
            )
        };
    }

    #[test]
    fn format_parameters() {
        assert_format!("{}{}{}", 5, 6, 7);
    }

    #[test]
    fn format_positional_parameters() {
        assert_format!("{2}{1}{0}", 5, 6, 7);
    }

    #[test]
    fn format_named_parameters() {
        let param_1 = 5;
        let param_2 = 6;
        let param_3 = 7;

        assert_eq!(
            format!("{param_2}{param_1}{param_3}"),
            rformat!("{param_2}{param_1}{param_3}", param_1, param_2, param_3).unwrap()
        );
    }

    #[test]
    fn format_width() {
        let width = 10;

        assert_format!("{:5}{:3$}{:w$}", 5, 6, 7, 3, w = width);
    }

    #[test]
    fn format_precision() {
        let precision: usize = 3;

        assert_format!("{0:.5} {0:.1$} {0:.p$}", 123.456789, 2, p = precision);
    }

    #[test]
    fn test_alignment_specifications() {
        // Test left, right, and center alignment with width
        assert_format!("{:<5}", 42);
        assert_format!("{:>5}", 42);
        assert_format!("{:^5}", 42);

        let array = [1, 2, 3];
        assert_format!("{:<5?}", array);
        assert_format!("{:>5?}", array);
        assert_format!("{:^5?}", array);
    }

    #[test]
    fn test_fill_character() {
        // Test with different fill characters
        assert_format!("{:*>7}", 42);
        assert_format!("{:A<7}", 42);
        assert_format!("{:🎉^7}", 42);

        let array = [1, 2, 3];
        assert_format!("{:*<5?}", array);
        assert_format!("{:A>5?}", array);
        assert_format!("{:🎉^5?}", array);
    }

    #[test]
    fn test_number_formatting() {
        // Test sign specifications
        assert_format!("{:+}", 42);
        assert_format!("{:+}", -42i64);
        assert_format!("{:-}", 42);
        assert_format!("{:-}", -42i64);
        assert_format!("{}", 42i64);
        assert_format!("{}", -42i64);

        // Test alternate form (#)
        assert_format!("{:#o}", 42);
        assert_format!("{:#x}", 42);

        // Test zero padding
        assert_format!("{:04}", 42);

        // Test precision with floating point
        assert_format!("{:.2}", 42.5);
    }

    #[test]
    fn test_all_format_types() {
        // Create test values
        let integer = 42;
        let negative = -42i32;
        let float = 3.14159;
        let string = "hello";
        let character = 'A';
        let boolean = true;
        let array = [1, 2, 3];

        // Test Display format
        assert_format!("{:}", integer);
        assert_format!("{}", boolean);

        // Test Debug format
        assert_format!("{:?}", integer);
        assert_format!("{:?}", string);
        assert_format!("{:?}", array);

        // Test LowerHex format
        assert_format!("{:x}", integer);
        assert_format!("{:x}", negative);

        // Test UpperHex format
        assert_format!("{:X}", integer);
        assert_format!("{:X}", negative);

        // Test Octal format
        assert_format!("{:o}", integer);

        // Test Binary format
        assert_format!("{:b}", integer);

        // Test LowerExp format
        assert_format!("{:e}", float);

        // Test UpperExp format
        assert_format!("{:E}", float);

        // Test Pointer format.
        let ptr = &character;
        assert_format!("{:p}", ptr);
    }

    #[test]
    fn test_precision_variations() {
        // Test precision on different types
        let float = 3.14159;
        let string = "hello";

        // Float precision
        assert_format!("{:.3}", float);
        assert_format!("{:.1}", float);
        assert_format!("{:.0}", float);

        // String truncation
        assert_format!("{:.2}", string);
        assert_format!("{:.10}", string);

        // Width and precision together
        assert_format!("{:6.2}", float);
        // assert_format!("{:6.3}", string);

        // Dynamic precision
        let precision = 2;
        assert_format!("{:.1$}", float, precision);
    }

    #[test]
    fn test_complex_combinations() {
        // Alignment + fill + width + sign + zero-padding
        assert_format!("{:+05}", 42);
        assert_format!("{:*^+7}", 42);

        // Alignment + width + precision
        assert_format!("{:7.2}", 3.14159);
        assert_format!("{:<7.2}", 3.14159);

        // # + zero padding + width + type
        assert_format!("{:#06x}", 42);
        assert_format!("{:#6x}", 42);
        assert_format!("{:#06X}", 42);

        // Fill + align + zero + width + precision
        assert_format!("{:06.2}", 3.14159);
        assert_format!("{:#>6.2}", 3.14159);

        // Complex alignment with sign
        assert_format!("{:>+5}", 42);
        assert_format!("{:^+5}", 42);
        assert_format!("{:^5}", -42i32);
    }

    #[test]
    fn test_sign_options() {
        // Sign options with positive numbers
        assert_format!("{}", 42);
        assert_format!("{:+}", 42);
        assert_format!("{:-}", 42);

        // Sign options with negative numbers
        assert_format!("{}", -42i64);
        assert_format!("{:+}", -42i64);
        assert_format!("{:-}", -42i64);

        // Sign options with zero
        assert_format!("{}", 0);
        assert_format!("{:+}", 0);
        assert_format!("{:-}", 0);
    }

    #[test]
    fn test_nested_formats() {
        // Test nested formats where width/precision come from parameters
        let width = 10;
        let precision = 2;
        let number = 3.14159;

        assert_format!("{:1$.2$}", number, width, precision);

        // Test width and precision that refer to other parameters
        assert_format!("{:>wi$.pre$}", number, wi = width, pre = precision);
    }

    #[test]
    fn test_alternate_forms() {
        // Test alternate forms for different types
        assert_format!("{:#b}", 42);
        assert_format!("{:#o}", 42);
        assert_format!("{:#x}", 42);
        assert_format!("{:#X}", 42);

        // Float alternate forms
        assert_format!("{:#}", 3.0);
        assert_format!("{:.0}", 3.0);
        assert_format!("{:#.0}", 3.0);
    }

    #[test]
    fn test_debug_formatting() {
        // Test various debug formatting options
        #[derive(Debug)]
        struct TestStruct {
            _field: i32,
        }

        #[derive(Debug)]
        struct TestStruct2 {
            _struct: TestStruct,
            _field: i32,
        }

        let test_struct = TestStruct2 {
            _struct: TestStruct { _field: 24 },
            _field: 42,
        };
        let test_struct_ref = &test_struct;

        // Basic debug formatting
        assert_format!("{:?}", test_struct_ref);

        // Debug with alternate form
        assert_format!("{:#?}", test_struct_ref);
        assert_format!("{:❌^#20?}", test_struct_ref);
    }

    #[test]
    fn test_unicode_fill() {
        // Test with Unicode fill characters
        assert_format!("{:♥>7}", 42);
        assert_format!("{:♥<7}", 42);
        assert_format!("{:♥^7}", 42);

        // Test with emoji
        assert_format!("{:😊^7}", 42);
    }

    #[test]
    fn test_format_errors() {
        // Test error cases
        assert!(rformat!("}", 42).is_err()); // Unmatched closing brace
        assert!(rformat!("{", 42).is_err()); // Unmatched opening brace
        assert!(rformat!("{} {}", 42).is_err()); // Out of bounds
        assert!(rformat!("{1}", 42).is_err()); // Index out of bounds
        assert!(rformat!("{unknown}", 42).is_err()); // Unknown named parameter
        assert!(rformat!("{:p}", 42).is_err()); // Unsupported format type
    }

    #[test]
    fn test_pointer() {
        let param = 42;
        let param_ref = &param;

        assert_format!("{:p}", param_ref);
    }
}
