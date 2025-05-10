use crate::error::{FormatError, Result};

#[derive(Debug, PartialEq)]
pub enum Argument<'f> {
    Integer(usize),
    Identifier(&'f str),
}

#[derive(Debug, PartialEq)]
pub struct FormatSpec<'f> {
    fill: Option<char>,
    align: Option<Align>,
    sign: Option<Sign>,
    alternate: bool,
    zero_pad: bool,
    width: Option<Count<'f>>,
    precision: Option<Precision<'f>>,
    r#type: Type,
}

#[derive(Debug, PartialEq)]
pub enum Align {
    Left,
    Center,
    Right,
}

#[derive(Debug, PartialEq)]
pub enum Sign {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum Precision<'f> {
    Count(Count<'f>),
    Star,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Binary,
    Debug,
    DebugLowerHex,
    DebugUpperHex,
    Display,
    LowerExp,
    LowerHex,
    Octal,
    Pointer,
    UpperExp,
    UpperHex,
}

#[derive(Debug, PartialEq)]
pub enum Count<'f> {
    Argument(Argument<'f>),
    Integer(usize),
}

pub fn parse_format_spec(mut format_spec: &str) -> Result<FormatSpec> {
    // Format spec found at https://doc.rust-lang.org/std/fmt/#syntax

    let mut format_spec_substr = format_spec;

    // Parse fill and alignment
    let (fill, align) = match (
        format_spec_substr.chars().nth(0),
        format_spec_substr.chars().nth(1),
    ) {
        (Some(fill), Some('<')) => (Some(fill), Some(Align::Left)),
        (Some(fill), Some('^')) => (Some(fill), Some(Align::Center)),
        (Some(fill), Some('>')) => (Some(fill), Some(Align::Right)),
        (Some('<'), _) => (None, Some(Align::Left)),
        (Some('^'), _) => (None, Some(Align::Center)),
        (Some('>'), _) => (None, Some(Align::Right)),
        _ => (None, None),
    };

    // Skip fill character
    if fill.is_some() {
        format_spec_substr = &format_spec_substr[1..];
    }

    // Skip align character
    if align.is_some() {
        format_spec_substr = &format_spec_substr[1..];
    }

    // Parse sign
    let sign = match format_spec_substr.chars().nth(0) {
        Some('+') => Some(Sign::Plus),
        Some('-') => Some(Sign::Minus),
        _ => None,
    };

    // Skip sign character
    if sign.is_some() {
        format_spec_substr = &format_spec_substr[1..];
    }

    // Parse alternate form (#)
    let alternate = match format_spec_substr.chars().nth(0) {
        Some('#') => true,
        _ => false,
    };

    // Skip alternate form character
    if alternate {
        format_spec_substr = &format_spec_substr[1..];
    }

    // Parse zero padding
    let zero_pad = match format_spec_substr.chars().nth(0) {
        Some('0') => true,
        _ => false,
    };

    // Skip zero pad character
    if zero_pad {
        format_spec_substr = &format_spec_substr[1..];
    }

    // Parse width

    // Here's the possibilities:
    // [width][.precision]type
    // type can be empty - for display.
    let dot = format_spec_substr.chars().position(|c| c == '.');
    let dollar = format_spec_substr.chars().position(|c| c == '$');
    let non_digit = format_spec_substr.chars().position(|c| !c.is_ascii_digit());

    let width = match (dot, dollar, non_digit) {
        // Have precision
        (Some(dot), _, _) => &format_spec_substr[..dot],
        // No precision, width is an argument
        (None, Some(dollar), _) => &format_spec_substr[..=dollar],
        // No precision, width is an integer - type at the end
        (None, None, Some(non_digit)) => &format_spec_substr[..non_digit],
        // No precision, width is an integer - type is empty
        _ => format_spec_substr,
    };

    format_spec_substr = &format_spec_substr[width.len()..];

    let width = parse_count(width)?;

    // Parse precision

    // Here's the possibilities:
    // [.precision]type
    // type can be empty - for display.
    let precision = if format_spec_substr.starts_with(".") {
        // skip dot
        format_spec_substr = &format_spec_substr[1..];

        if format_spec_substr.starts_with('*') {
            // skip star
            format_spec_substr = &format_spec_substr[1..];

            Some(Precision::Star)
        } else {
            let dollar = format_spec_substr.chars().position(|c| c == '$');
            let non_digit = format_spec_substr.chars().position(|c| !c.is_ascii_digit());

            let precision = match (dollar, non_digit) {
                // Precision is an argument
                (Some(dollar), _) => &format_spec_substr[..=dollar],
                // Precision is an integer - type at the end
                (None, Some(non_digit)) => &format_spec_substr[..non_digit],
                // Precision is an integer - type is empty
                _ => format_spec_substr,
            };

            format_spec_substr = &format_spec_substr[precision.len()..];

            parse_count(precision)?.map(|p| Precision::Count(p))
        }
    } else {
        // Option 2
        None
    };

    // Parse type
    let r#type = parse_type(format_spec_substr)?;

    Ok(FormatSpec {
        fill,
        align,
        sign,
        alternate,
        zero_pad,
        width,
        precision,
        r#type,
    })
}

fn parse_count(count: &str) -> Result<Option<Count<'_>>> {
    if count.is_empty() {
        return Ok(None);
    }

    // We have 3 options here:
    // 1. an integer - all characters until the precision or type are digits.
    // 2. an integer argument - all characters are digits, until a $ is reached.
    // 3. an identifier argument - regular characters until a $ is reached.

    if !count.ends_with('$') {
        // Option 1
        let parsed = count.parse::<usize>()?;

        Ok(Some(Count::Integer(parsed)))
    } else {
        // Options 2 & 3
        let argument = &count[..count.len() - 1];

        if argument.chars().all(|c| c.is_ascii_digit()) {
            // Option 2
            let parsed = argument.parse::<usize>()?;

            Ok(Some(Count::Argument(Argument::Integer(parsed))))
        } else {
            // Option 3
            Ok(Some(Count::Argument(Argument::Identifier(argument))))
        }
    }
}

fn parse_type(ty: &str) -> Result<Type> {
    Ok(match ty {
        "" => Type::Display,
        "?" => Type::Debug,
        "x?" => Type::DebugLowerHex,
        "X?" => Type::DebugUpperHex,
        "o" => Type::Octal,
        "x" => Type::LowerHex,
        "X" => Type::UpperHex,
        "p" => Type::Pointer,
        "b" => Type::Binary,
        "e" => Type::LowerExp,
        "E" => Type::UpperExp,
        _ => return Err(FormatError::UnsupportedFormatType(ty.to_string())),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_format_spec() {
        let spec = parse_format_spec("").unwrap();

        assert_eq!(spec.fill, None);
        assert_eq!(spec.align, None);
        assert_eq!(spec.sign, None);
        assert!(!spec.alternate);
        assert!(!spec.zero_pad);
        assert_eq!(spec.width, None);
        assert_eq!(spec.precision, None);
        assert_eq!(spec.r#type, Type::Display);
    }

    #[test]
    fn test_fill_and_align() {
        // Left align with _ as fill character
        let spec = parse_format_spec("_<").unwrap();
        assert_eq!(spec.fill, Some('_'));
        assert_eq!(spec.align, Some(Align::Left));

        // Center align with space as fill character
        let spec = parse_format_spec(" ^").unwrap();
        assert_eq!(spec.fill, Some(' '));
        assert_eq!(spec.align, Some(Align::Center));

        // Right align with 0 as fill character
        let spec = parse_format_spec("0>").unwrap();
        assert_eq!(spec.fill, Some('0'));
        assert_eq!(spec.align, Some(Align::Right));

        // Left align without fill character
        let spec = parse_format_spec("<").unwrap();
        assert_eq!(spec.fill, None);
        assert_eq!(spec.align, Some(Align::Left));

        // Center align without fill character
        let spec = parse_format_spec("^").unwrap();
        assert_eq!(spec.fill, None);
        assert_eq!(spec.align, Some(Align::Center));

        // Right align without fill character
        let spec = parse_format_spec(">").unwrap();
        assert_eq!(spec.fill, None);
        assert_eq!(spec.align, Some(Align::Right));
    }

    #[test]
    fn test_sign() {
        // Plus sign
        let spec = parse_format_spec("+").unwrap();
        assert_eq!(spec.sign, Some(Sign::Plus));

        // Minus sign
        let spec = parse_format_spec("-").unwrap();
        assert_eq!(spec.sign, Some(Sign::Minus));
    }

    #[test]
    fn test_alternate_form() {
        // With alternate form
        let spec = parse_format_spec("#").unwrap();
        assert!(spec.alternate);

        // Without alternate form
        let spec = parse_format_spec("").unwrap();
        assert!(!spec.alternate);
    }

    #[test]
    fn test_zero_padding() {
        // With zero padding
        let spec = parse_format_spec("0").unwrap();
        assert!(spec.zero_pad);

        // Without zero padding
        let spec = parse_format_spec("").unwrap();
        assert!(!spec.zero_pad);
    }

    #[test]
    fn test_width() {
        // Integer width
        let spec = parse_format_spec("10").unwrap();
        assert_eq!(spec.width, Some(Count::Integer(10)));

        // Integer argument
        let spec = parse_format_spec("1$").unwrap();
        assert_eq!(spec.width, Some(Count::Argument(Argument::Integer(1))));

        // Identifier argument
        let spec = parse_format_spec("width$").unwrap();
        assert_eq!(
            spec.width,
            Some(Count::Argument(Argument::Identifier("width")))
        );
    }

    #[test]
    fn test_precision() {
        // Integer precision
        let spec = parse_format_spec(".5").unwrap();
        assert_eq!(spec.precision, Some(Precision::Count(Count::Integer(5))));

        // Integer argument precision
        let spec = parse_format_spec(".2$").unwrap();
        assert_eq!(
            spec.precision,
            Some(Precision::Count(Count::Argument(Argument::Integer(2))))
        );

        // Identifier argument precision
        let spec = parse_format_spec(".prec$").unwrap();
        assert_eq!(
            spec.precision,
            Some(Precision::Count(Count::Argument(Argument::Identifier(
                "prec"
            ))))
        );

        // Star precision
        let spec = parse_format_spec(".*").unwrap();
        assert_eq!(spec.precision, Some(Precision::Star));

        // No precision
        let spec = parse_format_spec("").unwrap();
        assert_eq!(spec.precision, None);
    }

    #[test]
    fn test_type() {
        // Default type (Display)
        let spec = parse_format_spec("").unwrap();
        assert_eq!(spec.r#type, Type::Display);

        // Debug type
        let spec = parse_format_spec("?").unwrap();
        assert_eq!(spec.r#type, Type::Debug);

        // Debug lower hex
        let spec = parse_format_spec("x?").unwrap();
        assert_eq!(spec.r#type, Type::DebugLowerHex);

        // Debug upper hex
        let spec = parse_format_spec("X?").unwrap();
        assert_eq!(spec.r#type, Type::DebugUpperHex);

        // Octal
        let spec = parse_format_spec("o").unwrap();
        assert_eq!(spec.r#type, Type::Octal);

        // Lower hex
        let spec = parse_format_spec("x").unwrap();
        assert_eq!(spec.r#type, Type::LowerHex);

        // Upper hex
        let spec = parse_format_spec("X").unwrap();
        assert_eq!(spec.r#type, Type::UpperHex);

        // Pointer
        let spec = parse_format_spec("p").unwrap();
        assert_eq!(spec.r#type, Type::Pointer);

        // Binary
        let spec = parse_format_spec("b").unwrap();
        assert_eq!(spec.r#type, Type::Binary);

        // Lower exp
        let spec = parse_format_spec("e").unwrap();
        assert_eq!(spec.r#type, Type::LowerExp);

        // Upper exp
        let spec = parse_format_spec("E").unwrap();
        assert_eq!(spec.r#type, Type::UpperExp);

        // Unsupported type
        assert!(matches!(
            parse_format_spec("Z"),
            Err(FormatError::UnsupportedFormatType(_))
        ));
    }

    #[test]
    fn test_combined_format_specs() {
        // Fill, align, sign, alternate, zero pad, width, precision, type
        let spec = parse_format_spec("_>+#010.5x").unwrap();

        assert_eq!(spec.fill, Some('_'));
        assert_eq!(spec.align, Some(Align::Right));
        assert_eq!(spec.sign, Some(Sign::Plus));
        assert!(spec.alternate);
        assert!(spec.zero_pad);
        assert_eq!(spec.width, Some(Count::Integer(10)));
        assert_eq!(spec.precision, Some(Precision::Count(Count::Integer(5))));
        assert_eq!(spec.r#type, Type::LowerHex);

        // Left align with identifier argument width and identifier argument precision
        let spec = parse_format_spec("<width$.prec$?").unwrap();

        assert_eq!(spec.align, Some(Align::Left));
        assert_eq!(
            spec.width,
            Some(Count::Argument(Argument::Identifier("width")))
        );
        assert_eq!(
            spec.precision,
            Some(Precision::Count(Count::Argument(Argument::Identifier(
                "prec"
            ))))
        );
        assert_eq!(spec.r#type, Type::Debug);

        // Center align with integer argument width and star precision
        let spec = parse_format_spec("^1$.*").unwrap();
        assert_eq!(spec.width, Some(Count::Argument(Argument::Integer(1))));
        assert_eq!(spec.precision, Some(Precision::Star));
    }

    #[test]
    fn test_invalid_format_specs() {
        // This test assumes certain error handling logic
        // Test case for the current error in parse_width when format_spec_substr is empty
        assert!(parse_format_spec("invalid format").is_err());
        assert!(parse_format_spec("0invalid_width").is_err());
        assert!(parse_format_spec("0.invalid_precision").is_err());
    }
}
