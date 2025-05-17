use crate::error::{FormatError, Result};
use crate::format_spec::{self, Align, Argument, Count, FormatSpec, Precision, Sign};
use crate::prelude::*;

pub struct Parameter<'ident, 'formattable> {
    identifier: &'ident str,
    formattable: Formattable<'formattable>,
}

#[macro_export]
macro_rules! param {
    ($param_name:ident = $param:expr) => {
        $crate::format::Parameter {
            identifier: stringify!($param_name),
            formattable: into_formattable!($param),
        }
    };
    ($param:expr) => {
        $crate::format::Parameter {
            identifier: stringify!($param),
            formattable: into_formattable!($param),
        }
    };
}

#[macro_export]
macro_rules! params {
    ($($params:expr),* $(,)?) => (
        vec![$(param![$params]),*]
    );
}

pub fn rformat(fmt: &str, params: &[Parameter]) -> Result<String> {
    let mut result = String::new();

    let mut param_iter = params.iter();

    let mut chars = fmt.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        if c == '{' {
            if let Some((_, '{')) = chars.peek() {
                // Escaped parentheses `{{`
                chars.next();
                result.push('{');
            } else {
                let mut format = String::new();

                for (_, next) in chars.by_ref() {
                    if next == '}' {
                        break;
                    }

                    format.push(next);
                }

                write(&mut result, &format, params, &mut param_iter)?;
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
struct Format<'format_string> {
    argument: Option<Argument<'format_string>>,
    format_spec: FormatSpec<'format_string>,
}

fn write<
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

    // In case of `Precision::Star`, we have to extract the "precision" before the parameter, as it
    // cna influence the params iterator.
    // In other cases, order doesn't matter.
    // See "3. An asterisk .*" in  https://doc.rust-lang.org/std/fmt/#precision
    let precision = get_precision_from_params(&format_spec.precision, app_params, params_iter)?;
    let parameter = get_parameter_from_params(&format.argument, app_params, params_iter)?;

    let width = get_width_from_params(&format_spec, app_params)?
        // Setting width to 0 will make it have no effect.
        .unwrap_or(0);

    let as_display = parameter
        .formattable
        .try_as_display()
        .ok_or(std::fmt::Error)?;

    let out = match (&format_spec.align, &format_spec.sign, format_spec.alternate, format_spec.zero_pad, precision) {
        (Align::Left, None, true, true, None) => format!("{:<#0width$}", as_display),
        (Align::Left, None, true, true, Some(p)) => format!("{:<#0width$.p$}", as_display),
        (Align::Left, None, true, false, None) => format!("{:\u{001A}<#width$}", as_display),
        (Align::Left, None, true, false, Some(p)) => format!("{:\u{001A}<#width$.p$}", as_display),
        (Align::Left, None, false, true, None) => format!("{:<0width$}", as_display),
        (Align::Left, None, false, true, Some(p)) => format!("{:<0width$.p$}", as_display),
        (Align::Left, None, false, false, None) => format!("{:\u{001A}<width$}", as_display),
        (Align::Left, None, false, false, Some(p)) => format!("{:\u{001A}<width$.p$}", as_display),
        (Align::Left, Some(Sign::Plus), true, true, None) => format!("{:<+#0width$}", as_display),
        (Align::Left, Some(Sign::Minus), true, true, None) => format!("{:<-#0width$}", as_display),
        (Align::Left, Some(Sign::Plus), true, true, Some(p)) => format!("{:<+#0width$.p$}", as_display),
        (Align::Left, Some(Sign::Minus), true, true, Some(p)) => format!("{:<-#0width$.p$}", as_display),
        (Align::Left, Some(Sign::Plus), true, false, None) => format!("{:\u{001A}<+#width$}", as_display),
        (Align::Left, Some(Sign::Minus), true, false, None) => format!("{:\u{001A}<-#width$}", as_display),
        (Align::Left, Some(Sign::Plus), true, false, Some(p)) => format!("{:\u{001A}<+#width$.p$}", as_display),
        (Align::Left, Some(Sign::Minus), true, false, Some(p)) => format!("{:\u{001A}<-#width$.p$}", as_display),
        (Align::Left, Some(Sign::Plus), false, true, None) => format!("{:<+0width$}", as_display),
        (Align::Left, Some(Sign::Minus), false, true, None) => format!("{:<-0width$}", as_display),
        (Align::Left, Some(Sign::Plus), false, true, Some(p)) => format!("{:<+0width$.p$}", as_display),
        (Align::Left, Some(Sign::Minus), false, true, Some(p)) => format!("{:<-0width$.p$}", as_display),
        (Align::Left, Some(Sign::Plus), false, false, None) => format!("{:\u{001A}<+width$}", as_display),
        (Align::Left, Some(Sign::Minus), false, false, None) => format!("{:\u{001A}<-width$}", as_display),
        (Align::Left, Some(Sign::Plus), false, false, Some(p)) => format!("{:\u{001A}<+width$.p$}", as_display),
        (Align::Left, Some(Sign::Minus), false, false, Some(p)) => format!("{:\u{001A}<-width$.p$}", as_display),
        (Align::Center, None, true, true, None) => format!("{:^#0width$}", as_display),
        (Align::Center, None, true, true, Some(p)) => format!("{:^#0width$.p$}", as_display),
        (Align::Center, None, true, false, None) => format!("{:\u{001A}^#width$}", as_display),
        (Align::Center, None, true, false, Some(p)) => format!("{:\u{001A}^#width$.p$}", as_display),
        (Align::Center, None, false, true, None) => format!("{:^0width$}", as_display),
        (Align::Center, None, false, true, Some(p)) => format!("{:^0width$.p$}", as_display),
        (Align::Center, None, false, false, None) => format!("{:\u{001A}^width$}", as_display),
        (Align::Center, None, false, false, Some(p)) => format!("{:\u{001A}^width$.p$}", as_display),
        (Align::Center, Some(Sign::Plus), true, true, None) => format!("{:^+#0width$}", as_display),
        (Align::Center, Some(Sign::Minus), true, true, None) => format!("{:^-#0width$}", as_display),
        (Align::Center, Some(Sign::Plus), true, true, Some(p)) => format!("{:^+#0width$.p$}", as_display),
        (Align::Center, Some(Sign::Minus), true, true, Some(p)) => format!("{:^-#0width$.p$}", as_display),
        (Align::Center, Some(Sign::Plus), true, false, None) => format!("{:\u{001A}^+#width$}", as_display),
        (Align::Center, Some(Sign::Minus), true, false, None) => format!("{:\u{001A}^-#width$}", as_display),
        (Align::Center, Some(Sign::Plus), true, false, Some(p)) => format!("{:\u{001A}^+#width$.p$}", as_display),
        (Align::Center, Some(Sign::Minus), true, false, Some(p)) => format!("{:\u{001A}^-#width$.p$}", as_display),
        (Align::Center, Some(Sign::Plus), false, true, None) => format!("{:^+0width$}", as_display),
        (Align::Center, Some(Sign::Minus), false, true, None) => format!("{:^-0width$}", as_display),
        (Align::Center, Some(Sign::Plus), false, true, Some(p)) => format!("{:^+0width$.p$}", as_display),
        (Align::Center, Some(Sign::Minus), false, true, Some(p)) => format!("{:^-0width$.p$}", as_display),
        (Align::Center, Some(Sign::Plus), false, false, None) => format!("{:\u{001A}^+width$}", as_display),
        (Align::Center, Some(Sign::Minus), false, false, None) => format!("{:\u{001A}^-width$}", as_display),
        (Align::Center, Some(Sign::Plus), false, false, Some(p)) => format!("{:\u{001A}^+width$.p$}", as_display),
        (Align::Center, Some(Sign::Minus), false, false, Some(p)) => format!("{:\u{001A}^-width$.p$}", as_display),
        (Align::Right, None, true, true, None) => format!("{:>#0width$}", as_display),
        (Align::Right, None, true, true, Some(p)) => format!("{:>#0width$.p$}", as_display),
        (Align::Right, None, true, false, None) => format!("{:\u{001A}>#width$}", as_display),
        (Align::Right, None, true, false, Some(p)) => format!("{:\u{001A}>#width$.p$}", as_display),
        (Align::Right, None, false, true, None) => format!("{:>0width$}", as_display),
        (Align::Right, None, false, true, Some(p)) => format!("{:>0width$.p$}", as_display),
        (Align::Right, None, false, false, None) => format!("{:\u{001A}>width$}", as_display),
        (Align::Right, None, false, false, Some(p)) => format!("{:\u{001A}>width$.p$}", as_display),
        (Align::Right, Some(Sign::Plus), true, true, None) => format!("{:>+#0width$}", as_display),
        (Align::Right, Some(Sign::Minus), true, true, None) => format!("{:>-#0width$}", as_display),
        (Align::Right, Some(Sign::Plus), true, true, Some(p)) => format!("{:>+#0width$.p$}", as_display),
        (Align::Right, Some(Sign::Minus), true, true, Some(p)) => format!("{:>-#0width$.p$}", as_display),
        (Align::Right, Some(Sign::Plus), true, false, None) => format!("{:\u{001A}>+#width$}", as_display),
        (Align::Right, Some(Sign::Minus), true, false, None) => format!("{:\u{001A}>-#width$}", as_display),
        (Align::Right, Some(Sign::Plus), true, false, Some(p)) => format!("{:\u{001A}>+#width$.p$}", as_display),
        (Align::Right, Some(Sign::Minus), true, false, Some(p)) => format!("{:\u{001A}>-#width$.p$}", as_display),
        (Align::Right, Some(Sign::Plus), false, true, None) => format!("{:>+0width$}", as_display),
        (Align::Right, Some(Sign::Minus), false, true, None) => format!("{:>-0width$}", as_display),
        (Align::Right, Some(Sign::Plus), false, true, Some(p)) => format!("{:>+0width$.p$}", as_display),
        (Align::Right, Some(Sign::Minus), false, true, Some(p)) => format!("{:>-0width$.p$}", as_display),
        (Align::Right, Some(Sign::Plus), false, false, None) => format!("{:\u{001A}>+width$}", as_display),
        (Align::Right, Some(Sign::Minus), false, false, None) => format!("{:\u{001A}>-width$}", as_display),
        (Align::Right, Some(Sign::Plus), false, false, Some(p)) => format!("{:\u{001A}>+width$.p$}", as_display),
        (Align::Right, Some(Sign::Minus), false, false, Some(p)) => format!("{:\u{001A}>-width$.p$}", as_display),
    };

    let mut fill = [0;4];
    let out = out.replace("\u{001A}", format_spec.fill.encode_utf8(&mut fill));

    write!(dst, "{out}")?;

    Ok(())
}

fn get_precision_from_params<
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
            Precision::Count(Count::Argument(a)) => find_parameter(a, params)?,
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

fn get_width_from_params<'param, 'ident, 'formattable>(
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
                Count::Argument(a) => find_parameter(a, params)?,
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

fn find_parameter<'param, 'ident, 'formattable>(
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

fn get_parameter_from_params<
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
        Some(Argument::Identifier(i)) => params
            .iter()
            .find(|a| a.identifier == *i)
            .ok_or_else(|| FormatError::NoNamedParameter(i.to_string()))?,
        Some(Argument::Integer(i)) => params
            .get(*i)
            .ok_or(FormatError::NoPositionalParameter(*i))?,
    })
}

/// Parses format `'{' [ argument ] [ ':' format_spec ] [ ws ] * '}'`
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
    use crate::format::rformat;
    use crate::prelude::*;

    #[test]
    fn format_parameters() {
        let params = params![5, 6, 7];

        assert_eq!("567", rformat("{}{}{}", &params).unwrap());
    }

    #[test]
    fn format_positional_parameters() {
        let params = params![5, 6, 7];

        assert_eq!("765", rformat("{2}{1}{0}", &params).unwrap());
    }

    #[test]
    fn format_named_parameters() {
        let param_1 = 5;
        let param_2 = 6;
        let param_3 = 7;

        let params = params![param_1, param_2, param_3];

        assert_eq!(
            "657",
            rformat("{param_2}{param_1}{param_3}", &params).unwrap()
        );

        assert_eq!(format!("{:<5.2}", 0.123456789), "0.12 ");
    }

    #[test]
    fn format_width() {
        let width = 10;
        let params = params![5, 6, 7, 3, width];

        assert_eq!(
            "    5  6         7",
            rformat("{:5}{:3$}{:width$}", &params).unwrap()
        );
    }

    #[test]
    fn format_precision() {
        let precision: usize = 3;
        let params = params![123.456789, 2, precision];

        assert_eq!(
            "123.45679 123.46 123.457",
            rformat("{0:.5} {0:.1$} {0:.precision$}", &params).unwrap()
        );
    }
}
