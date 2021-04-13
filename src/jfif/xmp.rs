//! Parsing for XMP segments.

use crate::{
    jfif::{JFIFMarkerCode, ParseableSegment},
    parse,
};
use nom::{bytes::complete::tag, error::context, number::complete::be_u16, sequence::tuple};

#[derive(Debug)]
pub struct XMPSegment {
    pub data_size: usize,
    pub data: Vec<u8>,
}

impl XMPSegment {
    // XMP segments use APP1 segments.
    const MARKER: JFIFMarkerCode = JFIFMarkerCode::APPm(0x01);

    // XMP data section header
    const DATA_SECTION_HEADER: &'static [u8] = b"http://ns.adobe.com/xap/1.0\x00";

    fn parse_data_section_header(i: parse::Input) -> parse::Result<&[u8]> {
        context("XMP section header", tag(Self::DATA_SECTION_HEADER))(i)
    }
}

impl ParseableSegment for XMPSegment {
    fn can_parse_segment(i: parse::Input) -> bool {
        // An XMP segment should start as follows:
        //
        //      APP1 marker + 2 size bytes + "http://ns.adobe.com/xap/1.0/\x00"
        //

        let result = tuple((
            tag(Self::MARKER.as_bytes()),
            be_u16,
            Self::parse_data_section_header,
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
        use nom::bytes::complete::take;

        let content_len = if data_size < Self::DATA_SECTION_HEADER.len() {
            0
        } else {
            data_size - Self::DATA_SECTION_HEADER.len()
        };
        let (i, (_, data)) = tuple((
            context("Data section header", tag(Self::DATA_SECTION_HEADER)),
            take(content_len),
        ))(i)?;

        let data = data.to_vec();
        let seg = XMPSegment { data_size, data };

        Ok((i, seg))
    }
}
