pub type Input<'a> = &'a [u8];
pub type Result<'a, O> = nom::IResult<Input<'a>, O, nom::error::VerboseError<Input<'a>>>;

/// Implements a `parse` function for an input enum, that allows it to be
/// parsed in terms of a numeric type.
///
/// Shamelessly borrowed from fasterthanlime's executable packer tutorial:
/// https://fasterthanli.me/series/making-our-own-executable-packer
#[macro_export]
macro_rules! impl_parse_for_enum {
    ($type: ident, $number_parser: ident) => {
        impl $type {
            pub fn parse(i: crate::parse::Input) -> crate::parse::Result<Self> {
                use nom::{
                    combinator::map_res,
                    error::{context, ErrorKind},
                    number::complete::$number_parser,
                };
                use std::convert::TryFrom;

                let parser = map_res($number_parser, |x| {
                    Self::try_from(x).map_err(|_| ErrorKind::Alt)
                });
                context(stringify!($type), parser)(i)
            }
        }
    };
}
