use crate::{
    error::{FormatError, Result},
    format_spec::FormatSpec,
};

use super::format::Parameter;

macro_rules! match_format {
    ($format_spec: ident, $precision: ident, $width: ident, $ty: literal, $parameter: ident) => {
        match (
            &$format_spec.align,
            &$format_spec.sign,
            $format_spec.alternate,
            $format_spec.zero_pad,
            $precision,
        ) {
            ($crate::format_spec::Align::Left, None, true, true, None) => {
                format!(concat!("{:<#01$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Left, None, true, true, Some(p)) => {
                format!(concat!("{:<#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            ($crate::format_spec::Align::Left, None, true, false, None) => {
                format!(concat!("{:\u{001A}<#1$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Left, None, true, false, Some(p)) => {
                format!(
                    concat!("{:\u{001A}<#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            ($crate::format_spec::Align::Left, None, false, true, None) => {
                format!(concat!("{:<01$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Left, None, false, true, Some(p)) => {
                format!(concat!("{:<01$.2$", $ty, "}"), $parameter, $width, p)
            }
            ($crate::format_spec::Align::Left, None, false, false, None) => {
                format!(concat!("{:\u{001A}<1$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Left, None, false, false, Some(p)) => {
                format!(concat!("{:\u{001A}<1$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                true,
                true,
                None,
            ) => {
                format!(concat!("{:<+#01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                true,
                true,
                None,
            ) => {
                format!(concat!("{:<-#01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                true,
                true,
                Some(p),
            ) => {
                format!(concat!("{:<+#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                true,
                true,
                Some(p),
            ) => {
                format!(concat!("{:<-#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                true,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}<+#1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                true,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}<-#1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                true,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}<+#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                true,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}<-#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                false,
                true,
                None,
            ) => {
                format!(concat!("{:<+01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                false,
                true,
                None,
            ) => {
                format!(concat!("{:<-01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                false,
                true,
                Some(p),
            ) => {
                format!(concat!("{:<+01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                false,
                true,
                Some(p),
            ) => {
                format!(concat!("{:<-01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                false,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}<+1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                false,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}<-1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Plus),
                false,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}<+1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Left,
                Some($crate::format_spec::Sign::Minus),
                false,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}<-1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            ($crate::format_spec::Align::Center, None, true, true, None) => {
                format!(concat!("{:^#01$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Center, None, true, true, Some(p)) => {
                format!(concat!("{:^#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            ($crate::format_spec::Align::Center, None, true, false, None) => {
                format!(concat!("{:\u{001A}^#1$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Center, None, true, false, Some(p)) => {
                format!(
                    concat!("{:\u{001A}^#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            ($crate::format_spec::Align::Center, None, false, true, None) => {
                format!(concat!("{:^01$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Center, None, false, true, Some(p)) => {
                format!(concat!("{:^01$.2$", $ty, "}"), $parameter, $width, p)
            }
            ($crate::format_spec::Align::Center, None, false, false, None) => {
                format!(concat!("{:\u{001A}^1$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Center, None, false, false, Some(p)) => {
                format!(concat!("{:\u{001A}^1$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                true,
                true,
                None,
            ) => {
                format!(concat!("{:^+#01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                true,
                true,
                None,
            ) => {
                format!(concat!("{:^-#01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                true,
                true,
                Some(p),
            ) => {
                format!(concat!("{:^+#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                true,
                true,
                Some(p),
            ) => {
                format!(concat!("{:^-#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                true,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}^+#1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                true,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}^-#1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                true,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}^+#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                true,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}^-#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                false,
                true,
                None,
            ) => {
                format!(concat!("{:^+01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                false,
                true,
                None,
            ) => {
                format!(concat!("{:^-01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                false,
                true,
                Some(p),
            ) => {
                format!(concat!("{:^+01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                false,
                true,
                Some(p),
            ) => {
                format!(concat!("{:^-01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                false,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}^+1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                false,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}^-1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Plus),
                false,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}^+1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Center,
                Some($crate::format_spec::Sign::Minus),
                false,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}^-1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            ($crate::format_spec::Align::Right, None, true, true, None) => {
                format!(concat!("{:>#01$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Right, None, true, true, Some(p)) => {
                format!(concat!("{:>#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            ($crate::format_spec::Align::Right, None, true, false, None) => {
                format!(concat!("{:\u{001A}>#1$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Right, None, true, false, Some(p)) => {
                format!(
                    concat!("{:\u{001A}>#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            ($crate::format_spec::Align::Right, None, false, true, None) => {
                format!(concat!("{:>01$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Right, None, false, true, Some(p)) => {
                format!(concat!("{:>01$.2$", $ty, "}"), $parameter, $width, p)
            }
            ($crate::format_spec::Align::Right, None, false, false, None) => {
                format!(concat!("{:\u{001A}>1$", $ty, "}"), $parameter, $width)
            }
            ($crate::format_spec::Align::Right, None, false, false, Some(p)) => {
                format!(concat!("{:\u{001A}>1$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                true,
                true,
                None,
            ) => {
                format!(concat!("{:>+#01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                true,
                true,
                None,
            ) => {
                format!(concat!("{:>-#01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                true,
                true,
                Some(p),
            ) => {
                format!(concat!("{:>+#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                true,
                true,
                Some(p),
            ) => {
                format!(concat!("{:>-#01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                true,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}>+#1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                true,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}>-#1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                true,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}>+#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                true,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}>-#1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                false,
                true,
                None,
            ) => {
                format!(concat!("{:>+01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                false,
                true,
                None,
            ) => {
                format!(concat!("{:>-01$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                false,
                true,
                Some(p),
            ) => {
                format!(concat!("{:>+01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                false,
                true,
                Some(p),
            ) => {
                format!(concat!("{:>-01$.2$", $ty, "}"), $parameter, $width, p)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                false,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}>+1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                false,
                false,
                None,
            ) => {
                format!(concat!("{:\u{001A}>-1$", $ty, "}"), $parameter, $width)
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Plus),
                false,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}>+1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
            (
                $crate::format_spec::Align::Right,
                Some($crate::format_spec::Sign::Minus),
                false,
                false,
                Some(p),
            ) => {
                format!(
                    concat!("{:\u{001A}>-1$.2$", $ty, "}"),
                    $parameter, $width, p
                )
            }
        }
    };
}

pub(crate) fn format_binary(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let as_binary = parameter.formattable.try_as_binary().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Binary")
    })?;

    Ok(match_format!(format_spec, precision, width, "b", as_binary))
}

pub(crate) fn format_debug(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let as_debug = parameter.formattable.try_as_debug().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Debug")
    })?;

    Ok(match_format!(format_spec, precision, width, "?", as_debug))
}

pub(crate) fn format_debug_lower_hex(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let as_debug = parameter.formattable.try_as_debug().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Debug")
    })?;

    Ok(match_format!(format_spec, precision, width, "x?", as_debug))
}

pub(crate) fn format_debug_upper_hex(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let debug = parameter.formattable.try_as_debug().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Debug")
    })?;

    Ok(match_format!(format_spec, precision, width, "X?", debug))
}

pub(crate) fn format_display(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let display = parameter.formattable.try_as_display().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Display")
    })?;

    Ok(match_format!(format_spec, precision, width, "", display))
}

pub(crate) fn format_lower_exp(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let lower_exp = parameter.formattable.try_as_lower_exp().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "LowerExp")
    })?;

    Ok(match_format!(format_spec, precision, width, "e", lower_exp))
}

pub(crate) fn format_lower_hex(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let lower_hex = parameter.formattable.try_as_lower_hex().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "LowerHex")
    })?;

    Ok(match_format!(format_spec, precision, width, "x", lower_hex))
}

pub(crate) fn format_octal(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let octal = parameter.formattable.try_as_octal().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Octal")
    })?;

    Ok(match_format!(format_spec, precision, width, "o", octal))
}

/// `PointerWrapper` provides a way to properly format pointers through trait objects.
///
/// When formatting a trait object (`&dyn Pointer`) with `{:p}`, Rust will print
/// the memory address of the trait object itself, not invoke the object's Pointer
/// implementation or print the pointer value it represents. This wrapper allows
/// calling the Pointer trait's formatting implementation to correctly print the
/// contained pointer value.
struct PointerWrapper<'p> {
    pointer: &'p dyn std::fmt::Pointer,
}

impl<'p> std::fmt::Pointer for PointerWrapper<'p> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pointer.fmt(f)
    }
}

pub(crate) fn format_pointer(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let pointer = parameter.formattable.try_as_pointer().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "Pointer")
    })?;

    let wrapper = PointerWrapper { pointer };

    Ok(match_format!(format_spec, precision, width, "p", wrapper))
}

pub(crate) fn format_upper_exp(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let upper_exp = parameter.formattable.try_as_upper_exp().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "UpperExp")
    })?;

    Ok(match_format!(format_spec, precision, width, "E", upper_exp))
}

pub(crate) fn format_upper_hex(
    format_spec: &FormatSpec,
    precision: Option<usize>,
    width: usize,
    parameter: &Parameter,
) -> Result<String> {
    let upper_hex = parameter.formattable.try_as_upper_hex().ok_or_else(|| {
        FormatError::TraitNotImplemented(parameter.identifier.to_string(), "UpperHex")
    })?;

    Ok(match_format!(format_spec, precision, width, "X", upper_hex))
}
