//! Traits and functions for dealing with JFIF data in an image.

mod marker_codes;
pub mod app0;

use crate::parse;
use nom::error::context;
use serde::{Serialize, Deserialize};
pub use marker_codes::JFIFMarkerCode;

pub trait ParseableSegment<'a>: Serialize + Deserialize<'a> {
    /// Returns the unique JFIF segment marker. Each JFIF segment in a JPEG image should start off
    /// with the bytes `[0xff, segment.marker()]`.
    ///
    /// If this function returns `None`, it is assumed that the implementor of `ParseableSegment` is
    /// able to match any segment marker. This is useful for "catch-all" segment types that are
    /// allowed to match arbitrary JFIF segments.
    fn marker() -> Option<JFIFMarkerCode> where Self: Sized;

    /// Return the segment parker for a parsed segment. If `Self::marker()` is not `None`, this
    /// should be equal to `Self::marker()`.
    fn parsed_marker(&self) -> JFIFMarkerCode;

    /// Returns the size (in bytes) of the JFIF segment's data section. If the segment doesn't have
    /// a data section, this function should return `None`.
    fn data_size(&self) -> Option<usize>;

    /// Returns the full size of the JFIF segment, including the size bytes and the magic bytes at
    /// the start of the segment.
    fn segment_size(&self) -> usize {
        match (self.data_size(), self.parsed_marker()) {
            // For SOI and EOI, the segment size only consists of the two magic bytes
            (_, JFIFMarkerCode::SOI) | (_, JFIFMarkerCode::EOI) => 2,

            // For SOS, the segment size is the data size plus two magic bytes
            (None, JFIFMarkerCode::SOS) => 2,
            (Some(sz), JFIFMarkerCode::SOS) => sz + 2,

            // For all other markers, the segment size is the data size plus two magic bytes plus
            // two size bytes
            (None, _) => 4,
            (Some(sz), _) => sz + 4,
        }
    }

    /// Parse the data bytes of the JFIF segment, returning a new instance of the
    /// `ParseableSegment` implementor. `magic` contains the marker bytes for the segment, and
    /// `size` is the size of the data section.
    fn parse_data_bytes(
        i: parse::Input,
        magic: JFIFMarkerCode,
        data_size: usize,
    ) -> parse::Result<Self>
    where
        Self: Sized;

    /// Parse the JFIF segment starting from the segment marker.
    fn parse(i: parse::Input) -> parse::Result<Self>
    where
        Self: Sized,
    {
        use nom::{
            bytes::complete::take, combinator::verify, number::complete::be_u16,
        };

        // If a marker is explicitly defined for this type, we ensure that the segment magic
        // matches with that marker. Otherwise, we match an arbitrary 16-bit big-endian integer.
        let (i, magic) = match Self::marker() {
            Some(m) => context("Segment magic", verify(JFIFMarkerCode::parse, |&x| x == m))(i)?,
            None => context("Segment magic", JFIFMarkerCode::parse)(i)?,
        };

        let (i, data, data_size) = match magic {
            // For the markers SOI and EOI, there isn't any data associated with the tag.
            JFIFMarkerCode::SOI | JFIFMarkerCode::EOI => (i, &i[0..0], 0),

            // For the marker SOS, the data includes the rest of the file minus the two bytes at
            // the end of the file corresponding to the EOI tag. We don't explicitly validate the
            // remaining length of the file here; if it is invalid, then we should get an error
            // down the road when we try to parse the EOI tag.
            JFIFMarkerCode::SOS => {
                let data_size = if i.len() <= 2 { 0 } else { i.len() - 2 };
                let (i, data) = context("SOS data", take(data_size))(i)?;
                (i, data, data_size)
            }

            // For all remaining tags, the next two bytes should specify the size of the data
            // section. We retrieve the size from those bytes, and then retrieve the data from that
            // size.
            _ => {
                // The data section size includes the two bytes that are used to store the size, so
                // the actual number of data bytes is the stored size minus two. This also means
                // that the stored size must always be >= 2.
                let mut parser = context("Data section size", verify(be_u16, |&x| x >= 2));
                let (i, data_size) = parser(i)?;
                let data_size = (data_size - 2) as usize;

                let (i, data) = context("Data section", take(data_size))(i)?;
                (i, data, data_size)
            }
        };

        let (_, result) = context("Data section parser", |x| {
            Self::parse_data_bytes(x, magic, data_size)
        })(data)?;
        Ok((i, result))
    }
}

/// A type implementing the `ParseableSegment` trait that can be used to match an arbitrary JFIF
/// segment.
#[derive(Debug, Serialize, Deserialize)]
pub struct UnknownJFIFSegment {
    pub magic: JFIFMarkerCode,
    pub data_size: usize,
    pub data: Vec<u8>,
}

impl ParseableSegment<'_> for UnknownJFIFSegment {
    fn marker() -> Option<JFIFMarkerCode> {
        None
    }

    fn parsed_marker(&self) -> JFIFMarkerCode {
        return self.magic
    }

    fn data_size(&self) -> Option<usize> {
        Some(self.data_size)
    }

    fn parse_data_bytes(
        i: parse::Input,
        magic: JFIFMarkerCode,
        data_size: usize,
    ) -> parse::Result<Self> {
        use nom::bytes::complete::take;

        // Since we don't know anything about the segment, we don't have any way of handling the
        // input bytes. So instead, we're just going to store all of the data bytes into an array
        // and return that data alongside the segment.
        let (i, data) = context("UnknownJFIFSegment data bytes", take(data_size))(i)?;
        let data = data.to_vec();
        let seg = UnknownJFIFSegment {
            magic,
            data_size,
            data,
        };

        Ok((i, seg))
    }
}

/// Defines a segment of the JPEG/JFIF image.
#[derive(Debug, Serialize, Deserialize)]
pub enum JFIFSegment {
    APP0(app0::APP0Segment),
    APP1(crate::exif::ExifData),
    Unknown(UnknownJFIFSegment),
}

impl JFIFSegment {
    pub fn parse(i: parse::Input) -> parse::Result<Self> {
        // Check the magic bytes of the next segment
        let (_, magic) = context("Magic bytes", JFIFMarkerCode::parse)(&i)?;

        let (i, seg) = match magic {
            JFIFMarkerCode::APPm(0x00) => {
                let (i, seg) = context("APP0 segment", app0::APP0Segment::parse)(i)?;
                (i, JFIFSegment::APP0(seg))
            }
            JFIFMarkerCode::APPm(0x01) => {
                let (i, seg) = context("APP1 segment", crate::exif::ExifData::parse)(i)?;
                (i, JFIFSegment::APP1(seg))
            }
            _ => {
                let (i, seg) = context("Unknown segment", UnknownJFIFSegment::parse)(i)?;
                (i, JFIFSegment::Unknown(seg))
            }
        };

        Ok((i, seg))
    }
}
