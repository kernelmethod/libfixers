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
