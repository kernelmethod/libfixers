//! Parsing for JFIF APP0 marker segments.

use crate::{
    impl_parse_for_enum,
    jfif::{JFIFMarkerCode, ParseableSegment},
    parse,
};
use derive_try_from_primitive::TryFromPrimitive;
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub enum APP0Segment {
    JFIF(JFIFData),
    JFXX(JFXXData),
}

impl APP0Segment {
    const MARKER: JFIFMarkerCode = JFIFMarkerCode::APPm(0x00);

    /// Parse the segment identifier in the first five bytes of the data section. This should be
    /// either "JFIF\x00" or "JFXX\x00", depending on what type of segment this is.
    fn parse_data_bytes_header(i: parse::Input) -> parse::Result<&[u8]> {
        use nom::{bytes::complete::take, combinator::verify, error::context};
        let mut parser = context(
            "APP0 segment identifier",
            verify(take(5usize), |x: &[u8]| {
                x == "JFIF\x00".as_bytes() || x == "JFXX\x00".as_bytes()
            }),
        );
        parser(i)
    }
}

impl ParseableSegment<'_> for APP0Segment {
    fn can_parse_segment(i: parse::Input) -> bool {
        use nom::{bytes::complete::tag, number::complete::be_u16, sequence::tuple};

        // We should be able to parse any segment that starts with the following bytes:
        //
        //      APP0 marker + 2 size bytes + "JFIF\x00" or "JFXX\x00"
        //
        let mut parser = tuple((
            tag(Self::MARKER.as_bytes()),
            be_u16,
            Self::parse_data_bytes_header,
        ));

        match parser(i) {
            Ok(_) => true,
            Err(_) => false,
        }
    }

    fn marker(&self) -> JFIFMarkerCode {
        Self::MARKER
    }

    fn data_size(&self) -> Option<usize> {
        match self {
            APP0Segment::JFIF(data) => Some(data.data_size),
            APP0Segment::JFXX(data) => Some(data.data_size),
        }
    }

    fn parse_data_bytes(
        i: parse::Input,
        _magic: JFIFMarkerCode,
        data_size: usize,
    ) -> parse::Result<Self> {
        use nom::{bytes::complete::take, combinator::verify, error::context, number::complete::{be_u8, be_u16}, sequence::tuple};

        // The next 5 bytes contain the segment identifier. This should be either "JFIF\x00" if
        // this is a JFIF APP0 segment, or "JFXX\x00" if it's a JFXX APP0 segment.
        let mut parser = context(
            "APP0 segment identifier",
            verify(take(5usize), |x: &[u8]| {
                x == "JFIF\x00".as_bytes() || x == "JFXX\x00".as_bytes()
            }),
        );
        let (i, identifier) = parser(i)?;

        // Now we handle the parsing differently based on what the identifier was
        let (i, segment) = if identifier == "JFIF\x00".as_bytes() {
            // JFIF APP0 segment
            let (i, (v, du, xd, yd, xt, yt)) = tuple((
                context("JFIF APP0 segment version", be_u16),
                context("JFIF APP0 segment density units", DensityUnits::parse),
                context("JFIF APP0 segment x-density", be_u16),
                context("JFIF APP0 segment y-density", be_u16),
                context("JFIF APP0 segment x-thumbnail", be_u8),
                context("JFIF APP0 segment y-thumbnail", be_u8),
            ))(i)?;

            let (i, thumbnail) = context("JFIF APP0 segment thumbnail", take(i.len()))(i)?;

            let data = JFIFData {
                data_size,
                version: v,
                density_units: du,
                x_density: xd,
                y_density: yd,
                x_thumbnail: xt,
                y_thumbnail: yt,
                thumbnail: thumbnail.to_vec(),
            };

            (i, APP0Segment::JFIF(data))
        } else {
            // JFXX APP0 segment
            let (i, thumbnail_format) = context("JFXX APP0 segment thumbnail format", ThumbnailFormat::parse)(i)?;
            let (i, thumbnail) = context("JFXX APP0 segment thumbnail", take(i.len()))(i)?;
            let data = JFXXData {
                data_size,
                thumbnail_format,
                thumbnail: thumbnail.to_vec(),
            };
            (i, APP0Segment::JFXX(data))
        };

        Ok((i, segment))
    }
}

/// Data stored in a JFIF APP0 segment.
#[derive(Debug, Serialize, Deserialize)]
pub struct JFIFData {
    data_size: usize,
    version: u16,
    density_units: DensityUnits,
    x_density: u16,
    y_density: u16,
    x_thumbnail: u8,
    y_thumbnail: u8,
    thumbnail: Vec<u8>,
}

/// Data stored in a JFXX APP0 segment.
#[derive(Debug, Serialize, Deserialize)]
pub struct JFXXData {
    data_size: usize,
    thumbnail_format: ThumbnailFormat,
    thumbnail: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[derive(Serialize, Deserialize)]
#[repr(u8)]
pub enum DensityUnits {
    NoUnits = 0,
    PixelsPerInch = 1,
    PixelsPerCentimeter = 2,
}

impl_parse_for_enum!(DensityUnits, le_u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[derive(Serialize, Deserialize)]
#[repr(u8)]
pub enum ThumbnailFormat {
    JPEG = 10,
    Palettized = 11,
    RGB = 13,
}

impl_parse_for_enum!(ThumbnailFormat, le_u8);
