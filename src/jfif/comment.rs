//! Parsing for comment sections in JPEG files.

use crate::{
    jfif::{JFIFMarkerCode, ParseableSegment},
    parse,
};
use nom::{
    bytes::complete::take, combinator::verify, error::context, number::complete::be_u16,
    sequence::tuple,
};

#[derive(Debug)]
pub struct JPEGComment {
    pub data_size: usize,
    pub comment: String,
}

impl JPEGComment {
    const MARKER: JFIFMarkerCode = JFIFMarkerCode::COM;
}

impl ParseableSegment for JPEGComment {
    fn can_parse_segment(i: parse::Input) -> bool {
        // A comment segment should start as follows:
        //
        //      COM marker + 2 size bytes
        //

        let result = tuple((
            verify(JFIFMarkerCode::parse, |&x| x == Self::MARKER),
            be_u16,
        ))(i);
        result.is_ok()
    }

    fn marker(&self) -> JFIFMarkerCode {
        Self::MARKER
    }

    fn data_size(&self) -> Option<usize> {
        Some(self.data_size)
    }

    fn parse_data_bytes(
        i: parse::Input,
        _magic: JFIFMarkerCode,
        data_size: usize,
    ) -> parse::Result<Self> {
        let (i, comment) = context("Parse comment", take(data_size))(i)?;
        let comment = String::from_utf8_lossy(comment).to_string();
        let result = JPEGComment { data_size, comment };
        Ok((i, result))
    }
}

#[cfg(test)]
mod test {
    use super::JPEGComment;
    use crate::jfif::{JFIFMarkerCode, ParseableSegment};

    #[test]
    pub fn test_parse_comment() {
        let run_test = |msg: &str| {
            // Create a valid COM section using the input message
            let bmsg = msg.as_bytes();
            let c = vec![
                JFIFMarkerCode::COM.as_bytes().to_vec(),
                // Recall that the data section size = number of data bytes + 2 bytes to represent
                // the size of the section
                ((bmsg.len() + 2) as u16).to_be_bytes().to_vec(),
                bmsg.to_vec(),
            ]
            .concat();

            assert!(JPEGComment::can_parse_segment(&c));
            let (_, jc) = JPEGComment::parse(&c).unwrap();
            assert_eq!(jc.comment, msg);
        };

        run_test("hello, world!");
        run_test("\x00goodbye\x00");
        run_test("你好，世界!");
    }
}
