pub type Input<'a> = &'a [u8];
pub type Result<'a, O> = nom::IResult<Input<'a>, O, nom::error::VerboseError<Input<'a>>>;

/// Takes errors generated by parsing and extracts all of their messages into a vector, ordered
/// from higher-level contexts to lower-level contexts. In addition, the messages are modified to
/// show where the error occurred in the file that was being parsed.
pub fn extract_error_messages(original_input: Input, err: &nom::Err<nom::error::VerboseError<Input>>) -> Option<Vec<String>> {
    use nom::{error::VerboseErrorKind::Context, Offset};

    match err {
        nom::Err::Failure(e) | nom::Err::Error(e) => {
            // String together the contexts and offsets where we encountered each error to
            // demonstrate the error hierarchy.
            let context_msgs = e.errors
                .iter()
                .map(|err| match err {
                    (i, Context(c)) => format!("{} (at 0x{:x?})", c, original_input.offset(i)),
                    (i, e) => format!("{:?} (at 0x{:x?})", e, original_input.offset(i)),
                })
                // Contexts deeper in the stack show up before contexts later in the stack. We
                // reverse the order so that we go from the highest-level context to the
                // lowest-level context.
                .rev()
                .collect::<Vec<_>>();

            Some(context_msgs)
        }
        nom::Err::Incomplete(_) => {
            // nom::Err::Incomplete doesn't contain an array of errors, so we can't extract error
            // messages from it in the same way as we would otherwise.
            None
        }
    }
}

/// Take an error returned by parsing and convert it into a nicely-formatted string that makes it
/// easier to determine the source of the error.
#[allow(dead_code)]
pub fn pretty_error_message(original_input: Input, err: nom::Err<nom::error::VerboseError<Input>>) -> String 
{
    let err_msgs = match extract_error_messages(original_input, &err) {
        Some(msgs) => msgs,
        // Only occurs if the error is of type nom::Err::Incomplete
        None => return format!("Incomplete error: {:?}", err),
    };

    let mut err_msgs = err_msgs.iter();
    match err_msgs.next() {
        Some(m1) => err_msgs.fold(m1.to_string(), |acc, x| format!("{} => {}", acc, x)),
        // Generally should not occur. This would indicate that the `errors` field of
        // nom::Err::Error or nom::Err::Failure was empty.
        None => "(no error messages found)".to_string()
    }
}

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
