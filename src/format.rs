use crate::error::{FormatError, Result};
use crate::format_spec::{self, Argument, FormatSpec};
use crate::prelude::*;

pub struct Arg<'n, 'f> {
    identifier: &'n str,
    formattable: Formattable<'f>,
}

#[macro_export]
macro_rules! arg {
    ($arg:expr) => {
        $crate::format::Arg {
            identifier: stringify!($arg),
            formattable: into_formattable!($arg),
        }
    };
}

#[macro_export]
macro_rules! args {
    ($($args:expr),* $(,)?) => (
        vec![$(arg![$args]),*]
    );
}

pub fn format(fmt: &str, args: &[Arg]) -> Result<String> {
    let mut result = String::new();

    let mut arg_iter = args.into_iter().cycle();

    let mut chars = fmt.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        if c == '{' {
            if let Some((_, '{')) = chars.peek() {
                // Escaped parentheses `{{`
                chars.next();
                result.push('{');
            } else {
                // Actual parameter, read until `}`
                if args.len() == 0 {
                    return Err(FormatError::NoParameter(i, fmt.to_string()));
                }

                let mut format = String::new();

                while let Some((_, next)) = chars.next() {
                    if next == '}' {
                        break;
                    }

                    format.push(next);
                }

                // Unwrap safety: we already checked that args.len() is not 0, and the iterator is
                // cyclic, so next should never fail.
                write(&mut result, &format, args, arg_iter.next().unwrap())?;
            }
        } else if c == '}' {
            if let Some((_, '}')) = chars.peek() {
                // Escaped closing parentheses `}}`
                chars.next();
                result.push('}');
            } else {
                // Non-escaped closing parentheses, with no opening parentheses.
                return Err(FormatError::ClosingParentheses(i, fmt.to_string()));
            }
        } else {
            // Regular character.
            result.push(c);
        }
    }

    Ok(result)
}

#[derive(Debug, PartialEq)]
struct Format<'f> {
    argument: Option<Argument<'f>>,
    format_spec: FormatSpec<'f>,
}

fn write<W: std::fmt::Write>(
    dst: &mut W,
    format: &str,
    args: &[Arg],
    current_arg: &Arg,
) -> Result<()> {
    let format = parse_format(format)?;

    let argument = match format.argument {
        Some(Argument::Identifier(i)) => args
            .iter()
            .find(|a| a.identifier == i)
            .ok_or_else(|| FormatError::NoNamedParameter(i.to_string()))?,
        Some(Argument::Integer(i)) => args.get(i).ok_or(FormatError::NoPositionalParameter(i))?,
        None => current_arg,
    };

    let as_display = argument
        .formattable
        .try_as_display()
        .ok_or(std::fmt::Error)?;

    write!(dst, "{}", as_display)?;

    Ok(())
}

fn parse_format(format: &str) -> Result<Format> {
    let (argument, format_spec) = format.split_once(':').unwrap_or((format, ""));

    let argument = if argument.is_empty() {
        None
    } else if argument.chars().all(|c| c.is_ascii_digit()) {
        Some(Argument::Integer(argument.parse::<usize>()?))
    } else {
        Some(Argument::Identifier(argument))
    };

    let format_spec = format_spec::parse_format_spec(format_spec)?;

    Ok(Format {
        argument,
        format_spec,
    })
}

#[cfg(test)]
mod tests {
    use crate::format::format;
    use crate::prelude::*;

    #[test]
    fn format_parameters() {
        let args = args![5, 6, 7];

        assert_eq!("567", format("{}{}{}", &args).unwrap());
    }

    #[test]
    fn format_positional_parameters() {
        let args = args![5, 6, 7];

        assert_eq!("765", format("{2}{1}{0}", &args).unwrap());
    }

    #[test]
    fn format_named_parameters() {
        let arg_1 = 5;
        let arg_2 = 6;
        let arg_3 = 7;

        let args = args![arg_1, arg_2, arg_3];

        assert_eq!("657", format("{arg_2}{arg_1}{arg_3}", &args).unwrap());
    }
}
