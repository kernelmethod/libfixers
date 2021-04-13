//! Traits and functions for dealing with JFIF data in an image.

mod app0;
mod comment;
mod marker_codes;
mod xmp;

pub use app0::{APP0Segment,JFIFData,JFXXData};
pub use comment::JPEGComment;
pub use marker_codes::JFIFMarkerCode;
pub use xmp::XMPSegment;
use crate::{exif::ExifData, parse};
use nom::error::context;
use std::fmt;

pub trait ParseableSegment {
    /// Returns `true` if we believe that this `ParseableSegment` can parse this segment of the
    /// input. Otherwise, returns `false`.
    fn can_parse_segment(i: parse::Input) -> bool
    where
        Self: Sized;

    /// Returns the segment marker for the parsed segment.
    fn marker(&self) -> JFIFMarkerCode;

    /// Returns the size (in bytes) of the JFIF segment's data section. If the segment doesn't have
    /// a data section, this function should return `None`.
    fn data_size(&self) -> Option<usize>;

    /// Returns the full size of the JFIF segment, including the size bytes and the magic bytes at
    /// the start of the segment.
    fn segment_size(&self) -> usize {
        match (self.data_size(), self.marker()) {
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
        use nom::{bytes::complete::take, combinator::verify, number::complete::be_u16};

        let (i, magic) = context("Segment magic", JFIFMarkerCode::parse)(i)?;

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
#[derive(Debug)]
pub struct UnknownJFIFSegment {
    pub magic: JFIFMarkerCode,
    pub data_size: usize,
    pub data: Vec<u8>,
}

impl ParseableSegment for UnknownJFIFSegment {
    fn can_parse_segment(i: parse::Input) -> bool {
        // We can parse arbitrary segments with this input so long as they begin with a valid
        // marker
        match JFIFMarkerCode::parse(i) {
            Ok(_) => true,
            Err(_) => false,
        }
    }

    fn marker(&self) -> JFIFMarkerCode {
        self.magic
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

/// An enum that wraps around different segment types that we can detect in a JPEG image.
pub enum JFIFSegment {
    APP0Segment(APP0Segment),
    CommentSegment(JPEGComment),
    ExifSegment(ExifData),
    XMPSegment(XMPSegment),
    Unknown(UnknownJFIFSegment),
}

impl fmt::Debug for JFIFSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JFIFSegment::APP0Segment(_) => write!(f, "APP0Segment"),
            JFIFSegment::CommentSegment(_) => write!(f, "CommentSegment"),
            JFIFSegment::ExifSegment(_) => write!(f, "ExifSegment"),
            JFIFSegment::XMPSegment(_) => write!(f, "XMPSegment"),
            JFIFSegment::Unknown(_) => write!(f, "Unknown segment"),
        }
    }
}

impl JFIFSegment {
    pub fn parse(i: parse::Input) -> parse::Result<Self> {
        // We run parsers over the next segment by first checking the segment magic, and then
        // running over parsers for segments that start with that magic in order of priority.
        let (_, magic) = context("Segment magic", JFIFMarkerCode::parse)(&i)?;

        macro_rules! parser_func {
            ($seg_type: ident, $data_type: ident) => {
                || -> parse::Result<Self> {
                    let (i, data) =
                        context(stringify!(JFIFSegment::$seg_type), $data_type::parse)(i)?;
                    Ok((i, JFIFSegment::$seg_type(data)))
                }
            };
        }

        let parse_comment = parser_func!(CommentSegment, JPEGComment);
        let parse_exif = parser_func!(ExifSegment, ExifData);
        let parse_xmp = parser_func!(XMPSegment, XMPSegment);
        let parse_unknown = parser_func!(Unknown, UnknownJFIFSegment);

        match magic {
            JFIFMarkerCode::APPm(0x01) => {
                if ExifData::can_parse_segment(&i) {
                    parse_exif()
                } else if XMPSegment::can_parse_segment(&i) {
                    parse_xmp()
                } else {
                    parse_unknown()
                }
            }

            JFIFMarkerCode::COM => {
                if JPEGComment::can_parse_segment(&i) {
                    parse_comment()
                } else {
                    parse_unknown()
                }
            }

            _ => parse_unknown(),
        }
    }

    pub fn data(&self) -> &dyn ParseableSegment {
        match self {
            JFIFSegment::APP0Segment(data) => data,
            JFIFSegment::CommentSegment(data) => data,
            JFIFSegment::ExifSegment(data) => data,
            JFIFSegment::XMPSegment(data) => data,
            JFIFSegment::Unknown(data) => data,
        }
    }
}
