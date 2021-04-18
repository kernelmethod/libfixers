//! Parsing for the Exif data structure.

pub mod gps;
mod tags;

use crate::{
    impl_parse_for_enum,
    jfif::{JFIFMarkerCode, ParseableSegment},
    parse,
};
use derive_try_from_primitive::TryFromPrimitive;
use nom::{bytes::complete::tag, error::context, sequence::tuple};
use serde::{ser::SerializeTuple, Deserialize, Serialize, Serializer};
use std::convert::TryFrom;
pub use tags::IFDTag;

/// The Exif data structure containing all of the metadata specified in
/// an Exif section.
#[derive(Debug, Serialize, Deserialize)]
pub struct ExifData {
    pub data_size: usize,
    pub tiff_header: TIFFHeader,
    pub ifds: Vec<IFD>,
}

impl ExifData {
    const MARKER: JFIFMarkerCode = JFIFMarkerCode::APPm(0x01);

    pub fn collect_ifd_entries(&self) -> Vec<IFDEntry> {
        let mut entries = Vec::new();
        for ifd in self.ifds.iter() {
            entries.append(&mut collect_ifd_entries(ifd))
        }
        entries
    }

    /// Verify that the data section contains the correct header bytes.
    fn parse_data_bytes_header(i: parse::Input) -> parse::Result<&[u8]> {
        use nom::{bytes::complete::take, combinator::verify};
        let mut parser = context(
            "Exif data section header",
            verify(take(6usize), |x: &[u8]| x == "Exif\x00\x00".as_bytes()),
        );
        parser(i)
    }

    /// Convert an IFD segment into a byte array
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut components = Vec::new();

        // Start by adding the "Exif\x00\x00" header that always comes at the start of an Exif
        // segment
        components.push(b"Exif\x00\x00".to_vec());

        // Add the TIFF header
        let header = self.tiff_header.as_bytes();
        let alignment = self.tiff_header.alignment;
        components.push(header.to_vec());

        // Now add the IFD entries.
        // TODO: consider sub-IFDs
        let mut tiff_header_offset = header.len();
        for ifd in self.ifds.iter() {
            // The offset passed to ifd.as_bytes is the offset of the IFD relative to the TIFF
            // header
            let ifd_data = ifd.as_bytes(alignment, tiff_header_offset);
            tiff_header_offset += ifd_data.len();
            components.push(ifd_data);
        }

        // Concatenate all of the components together and return them as a single byte array
        let data = components
            .iter()
            .flatten()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>();

        // Now add the segment header at the start of the data
        let marker = self.marker().as_bytes().to_vec();
        let data_size = ((data.len() + 2) as u16).to_be_bytes().to_vec();
        [marker, data_size, data].concat()
    }
}

impl ParseableSegment for ExifData {
    fn can_parse_segment(i: parse::Input) -> bool {
        use nom::number::complete::be_u16;

        // We should be able to parse this segment if the first few bytes match the following
        // pattern:
        //
        //      APP1 header + 2 size bytes + "Exif\x00\x00"
        //
        let mut parser = tuple((
            tag(Self::MARKER.as_bytes()),
            be_u16,
            Self::parse_data_bytes_header,
        ));
        parser(i).is_ok()
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
        // If this is an Exif section, the next six bytes should be "Exif\x00\x00"
        let (i, _) = context("Exif header", tag("Exif\x00\x00".as_bytes()))(i)?;

        let ifd_slice = &i;
        let (i, tiff_header) = context("TIFF header", TIFFHeader::parse)(i)?;

        // TODO: skip initial offset (will have to subtract offset from 0x8)

        // Parse the IFD
        let (i, ifd) = context("IFD", |x| IFD::parse(x, ifd_slice, tiff_header.alignment))(i)?;
        let ifds = vec![ifd];

        let exif_data = ExifData {
            data_size,
            tiff_header,
            ifds,
        };

        Ok((i, exif_data))
    }
}

/// TIFF header used within the Exif data structure to specify its layout.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct TIFFHeader {
    pub alignment: TIFFByteAlignment,
    pub initial_offset: u32,
}

impl TIFFHeader {
    pub fn parse(i: parse::Input) -> parse::Result<Self> {
        use nom::combinator::verify;

        let (i, alignment) = context("Byte alignment", TIFFByteAlignment::parse)(i)?;
        let (i, _) = context(
            "Alignment check",
            verify(|x| alignment.parse_u16(x), |&x| x == 0x002a),
        )(i)?;
        let (i, initial_offset) = context("Initial offset", |x| alignment.parse_u32(x))(i)?;

        Ok((
            i,
            TIFFHeader {
                alignment,
                initial_offset,
            },
        ))
    }

    pub fn as_bytes(&self) -> [u8; 8] {
        // Header consists of 12 bytes for alignment + alignment check + initial offset
        let (check, initial_offset) = match self.alignment {
            TIFFByteAlignment::LittleEndian => (
                (0x2au16).to_le_bytes(),
                (self.initial_offset as u32).to_le_bytes(),
            ),
            TIFFByteAlignment::BigEndian => (
                (0x2au16).to_be_bytes(),
                (self.initial_offset as u32).to_be_bytes(),
            ),
        };

        let alignment = (self.alignment as u16).to_be_bytes();

        // TODO: find a better way to convert multiple static-sized arrays into a single
        // static-sized array?
        let mut result = [0; 8];
        result[0..2].copy_from_slice(&alignment);
        result[2..4].copy_from_slice(&check);
        result[4..8].copy_from_slice(&initial_offset);

        result
    }
}

/// Two-byte tag representing the byte alignment for the TIFF data.
#[derive(Debug, Clone, Copy, TryFromPrimitive, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u16)]
pub enum TIFFByteAlignment {
    LittleEndian = 0x4949, // "II" = Intel-type byte alignment
    BigEndian = 0x4d4d,    // "MM" = Motorola-type byte alignment
}

macro_rules! TIFFByteAlignment_parse_numeric {
    ($fn_name: ident, $type: ident, $le_number_parser: ident, $be_number_parser: ident) => {
        impl TIFFByteAlignment {
            pub fn $fn_name<'a>(&self, i: parse::Input<'a>) -> parse::Result<'a, $type> {
                use nom::number::complete::{$be_number_parser, $le_number_parser};
                match self {
                    TIFFByteAlignment::LittleEndian => $le_number_parser(i),
                    TIFFByteAlignment::BigEndian => $be_number_parser(i),
                }
            }
        }
    };
}

impl_parse_for_enum!(TIFFByteAlignment, be_u16);
TIFFByteAlignment_parse_numeric!(parse_u8, u8, le_u8, be_u8);
TIFFByteAlignment_parse_numeric!(parse_i8, i8, le_i8, be_i8);
TIFFByteAlignment_parse_numeric!(parse_u16, u16, le_u16, be_u16);
TIFFByteAlignment_parse_numeric!(parse_i16, i16, le_i16, be_i16);
TIFFByteAlignment_parse_numeric!(parse_u32, u32, le_u32, be_u32);
TIFFByteAlignment_parse_numeric!(parse_i32, i32, le_i32, be_i32);
TIFFByteAlignment_parse_numeric!(parse_f32, f32, le_f32, be_f32);
TIFFByteAlignment_parse_numeric!(parse_f64, f64, le_f64, be_f64);

/// Encapsulates an IFD (Image File Directory) in the image metadata.
#[derive(Debug, Serialize, Deserialize)]
pub struct IFD {
    pub num_entries: u16,
    pub entries: Vec<IFDEntry>,
    pub subifds: Vec<Box<IFD>>,
    pub offset_to_next: Option<u32>,
}

impl IFD {
    pub fn parse<'a>(
        i: parse::Input<'a>,
        original_input: parse::Input<'a>,
        alignment: TIFFByteAlignment,
    ) -> parse::Result<'a, Self> {
        let (i, num_entries) = context("Number of IFD entries", |x| alignment.parse_u16(x))(i)?;
        let mut entries = Vec::new();
        let mut current_input = i;

        for _ in 0..num_entries {
            let (next_i, entry) = context("IFD entry", |i| {
                IFDEntry::parse(i, &original_input, alignment)
            })(current_input)?;
            current_input = next_i;
            entries.push(entry);
        }

        let i = current_input;
        let (i, offset_to_next) = context("Offset to next IFD", |x| alignment.parse_u32(x))(i)?;
        let offset_to_next = if offset_to_next == 0 {
            None
        } else {
            Some(offset_to_next)
        };

        // Check whether there's a sub-IFD. If there is, we read it before exiting.
        let mut subifds = Vec::new();
        for e in entries.iter().filter(|e| match e.tagtype {
            IFDTag::ExifOffset | IFDTag::GPSInfo => true,
            _ => false,
        }) {
            match &e.content.get(0) {
                Some(IFDDataContents::UnsignedLong(offset)) => {
                    // Found sub-IFD; we recursively parse it and extract its contents
                    let (_, subifd) = IFD::parse(
                        &original_input[(*offset as usize)..],
                        &original_input,
                        alignment,
                    )?;
                    subifds.push(Box::new(subifd))
                }
                _ => {}
            }
        }

        let ifd = IFD {
            num_entries,
            entries,
            subifds,
            offset_to_next,
        };
        Ok((i, ifd))
    }

    /// Convert an IFD to a byte array. `alignment` is the byte alignment being used by this Exif
    /// segment, and `offset` is the offset (in bytes) between the TIFF header and the start of
    /// this IFD.
    pub fn as_bytes(&self, alignment: TIFFByteAlignment, offset: usize) -> Vec<u8> {
        let mut components = Vec::new();

        // Start by adding the number of entries in the IFD
        let size = match alignment {
            TIFFByteAlignment::BigEndian => (self.entries.len() as u16).to_be_bytes(),
            TIFFByteAlignment::LittleEndian => (self.entries.len() as u16).to_le_bytes(),
        };
        components.push(size.to_vec());

        // Iterate over each IFD. If the size of the IFD's data is > 12 bytes, we store it after
        // all of the entries have been specified.
        //
        // Note that the initial offset for data is calculated as
        //
        //      data_offset = offset from TIFF header + 2 bytes for # of IFD entries
        //                      + (# of entries) * (12 bytes/IFD entry)
        //                      + 4 bytes for offset to next IFD
        //
        let mut data_offset = offset + 2 + self.entries.len() * 12 + 4;
        let mut ifd_data = Vec::new();
        for entry in self.entries.iter() {
            let mut data = match alignment {
                TIFFByteAlignment::BigEndian => {
                    components.push(entry.tagtype.to_be_bytes().to_vec());
                    components.push(entry.data_format.to_be_bytes().to_vec());
                    components.push(entry.n_components.to_be_bytes().to_vec());

                    // TODO: currently entry.content is allowed to contain data of multiple types.
                    // We either need to restrict this to just one type, or else handle the case
                    // where we have multiple data types somehow.
                    entry
                        .content
                        .iter()
                        .map(|x| x.to_be_bytes())
                        .flatten()
                        .map(|x| x.to_owned())
                        .collect::<Vec<_>>()
                }
                TIFFByteAlignment::LittleEndian => {
                    components.push(entry.tagtype.to_le_bytes().to_vec());
                    components.push(entry.data_format.to_le_bytes().to_vec());
                    components.push(entry.n_components.to_le_bytes().to_vec());

                    // TODO: currently entry.content is allowed to contain data of multiple types.
                    // We either need to restrict this to just one type, or else handle the case
                    // where we have multiple data types somehow.
                    entry
                        .content
                        .iter()
                        .map(|x| x.to_le_bytes())
                        .flatten()
                        .map(|x| x.to_owned())
                        .collect::<Vec<_>>()
                }
            };

            let data_size = entry.n_components * (entry.data_format.bytes_per_component() as u32);
            if data_size <= 4 {
                // Can put the data in the next four bytes of the IFD. If the size of the data is
                // strictly < 4, we'll need to pad it to get to four bytes.
                if data.len() < 4 {
                    data.append(&mut vec![0u8; 4 - data.len()]);
                }

                components.push(data);
            } else {
                // Have to put an offset to the data section in the next four bytes, and then add
                // the data to the data section.
                let offset = match alignment {
                    TIFFByteAlignment::BigEndian => (data_offset as u32).to_be_bytes(),
                    TIFFByteAlignment::LittleEndian => (data_offset as u32).to_le_bytes(),
                };
                components.push(offset.to_vec());
                data_offset += data.len();
                ifd_data.push(data);
            }
        }

        // Add the offset to the next IFD
        let offset_to_next = match (self.offset_to_next, alignment) {
            (None, _) => 0u32.to_be_bytes(),
            (Some(x), TIFFByteAlignment::BigEndian) => x.to_be_bytes(),
            (Some(x), TIFFByteAlignment::LittleEndian) => x.to_le_bytes(),
        };
        components.push(offset_to_next.to_vec());
        components.push(
            ifd_data
                .iter()
                .flatten()
                .map(|x| x.to_owned())
                .collect::<Vec<_>>(),
        );
        components
            .iter()
            .flatten()
            .map(|x| x.to_owned())
            .collect::<Vec<_>>()
    }
}

/// Recursively crawl an IFD and all linked IFDs to retrieve all of the `IFDEntry` instances they
/// contain and put them into a single `Vec`.
pub fn collect_ifd_entries(ifd: &IFD) -> Vec<IFDEntry> {
    let mut entries = ifd.entries.clone();
    for sub_ifd in ifd.subifds.iter() {
        entries.append(&mut collect_ifd_entries(&*sub_ifd))
    }

    entries
}

/// Represents a single IFD (Image File Directory) entry
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IFDEntry {
    pub tagtype: IFDTag,
    pub data_format: IFDDataFormat,
    pub n_components: u32,
    pub content: Vec<IFDDataContents>,
}

impl IFDEntry {
    pub fn parse<'a>(
        i: parse::Input<'a>,
        original_input: parse::Input<'a>,
        alignment: TIFFByteAlignment,
    ) -> parse::Result<'a, Self> {
        let (i, (tagtype, data_format, n_components)) = tuple((
            context("IFD tag", |x| IFDTag::parse_unknown(x, alignment)),
            context("Data format", |x| IFDDataFormat::parse(x, alignment)),
            context("Number of components", |x| alignment.parse_u32(x)),
        ))(i)?;

        let content_size = data_format.bytes_per_component() * (n_components as usize);

        // If the total data size is <= 4 bytes, then the data is stored within the next four
        // bytes. Otherwise, an offset is stored, and we have to extract the value by visiting that
        // offset.
        let mut parser = context("IFD entry contents", |x| {
            IFDDataContents::parse(x, data_format, n_components, alignment)
        });
        let (i, content) = if content_size <= 4 {
            let input_after_data = &i[4..];
            let (_, content) = parser(&i)?;
            (input_after_data, content)
        } else {
            let (i, offset) = context("IFD entry offset", |x| alignment.parse_u32(x))(i)?;
            let (_, content) = parser(&original_input[(offset as usize)..])?;
            (i, content)
        };

        Ok((
            i,
            IFDEntry {
                tagtype,
                data_format,
                n_components,
                content,
            },
        ))
    }
}

/// Represents all of the possible values that a ResolutionUnit IFD field can adopt.
#[derive(Debug, Clone, Copy, Eq, PartialEq, TryFromPrimitive)]
#[repr(u16)]
pub enum ResolutionUnit {
    NoUnit = 1,
    Inch = 2,
    Centimeter = 3,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, TryFromPrimitive, Serialize, Deserialize)]
#[repr(u16)]
pub enum IFDDataFormat {
    UnsignedByte = 1,
    AsciiString = 2,
    UnsignedShort = 3,
    UnsignedLong = 4,
    UnsignedRational = 5,
    SignedByte = 6,
    Undefined = 7,
    SignedShort = 8,
    SignedLong = 9,
    SignedRational = 10,
    SingleFloat = 11,
    DoubleFloat = 12,
}

impl IFDDataFormat {
    pub fn parse(i: parse::Input, alignment: TIFFByteAlignment) -> parse::Result<Self> {
        use nom::{combinator::map_res, error::ErrorKind};

        let parser = map_res(
            |x| alignment.parse_u16(x),
            |x| Self::try_from(x).map_err(|_| ErrorKind::Alt),
        );
        context("IFD Data Format", parser)(i)
    }

    pub fn bytes_per_component(&self) -> usize {
        match self {
            IFDDataFormat::UnsignedByte => 1,
            IFDDataFormat::AsciiString => 1,
            IFDDataFormat::UnsignedShort => 2,
            IFDDataFormat::UnsignedLong => 4,
            IFDDataFormat::UnsignedRational => 8,
            IFDDataFormat::SignedByte => 1,
            IFDDataFormat::Undefined => 1,
            IFDDataFormat::SignedShort => 2,
            IFDDataFormat::SignedLong => 4,
            IFDDataFormat::SignedRational => 8,
            IFDDataFormat::SingleFloat => 4,
            IFDDataFormat::DoubleFloat => 8,
        }
    }

    /// Return the memory representation of this `IFDDataFormat` as a byte array in little-endian
    /// byte order.
    pub fn to_le_bytes(self) -> [u8; 2] {
        (self as u16).to_le_bytes()
    }

    /// Return the memory representation of this `IFDDataFormat` as a byte array in big-endian byte
    /// order.
    pub fn to_be_bytes(self) -> [u8; 2] {
        (self as u16).to_be_bytes()
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
pub enum IFDDataContents {
    UnsignedByte(u8),
    AsciiString(String),
    UnsignedShort(u16),
    UnsignedLong(u32),
    UnsignedRational(u32, u32),
    SignedByte(i8),
    Undefined(Vec<u8>),
    SignedShort(i16),
    SignedLong(i32),
    SignedRational(i32, i32),
    SingleFloat(f32),
    DoubleFloat(f64),
}

impl IFDDataContents {
    pub fn parse(
        i: parse::Input,
        format: IFDDataFormat,
        n_components: u32,
        alignment: TIFFByteAlignment,
    ) -> parse::Result<Vec<Self>> {
        match format {
            IFDDataFormat::AsciiString | IFDDataFormat::Undefined => {
                // For both of these formats, n_components refers to the number of characters/bytes
                // in the data type, rather than the number of instances of the data type that
                // exist. Therefore, the logic for parsing these types is equivalent to the logic
                // for parsing a single instance of them.
                let (i, result) = Self::parse_one(i, format, n_components, alignment)?;
                Ok((i, vec![result]))
            }
            _ => {
                // For all other formats, n_components refers to the number of instances of the
                // type that exist. While in general we will find that n_components == 1, in some
                // cases (e.g. for GPS lat/lon data) there may be multiple components.
                let mut contents = Vec::new();
                let mut i = i;
                for _ in 0..n_components {
                    let (next_i, res) = Self::parse_one(i, format, n_components, alignment)?;
                    i = &next_i;
                    contents.push(res);
                }
                Ok((i, contents))
            }
        }
    }

    /// Parse a single value of the input data format.
    fn parse_one(
        i: parse::Input,
        format: IFDDataFormat,
        n_components: u32,
        alignment: TIFFByteAlignment,
    ) -> parse::Result<Self> {
        use nom::bytes::complete::{take, take_while};
        let n_components = n_components as usize;

        match format {
            IFDDataFormat::UnsignedByte => {
                let (i, x) =
                    context("IFDDataContents::UnsignedByte", |i| alignment.parse_u8(i))(i)?;
                Ok((i, IFDDataContents::UnsignedByte(x)))
            }
            IFDDataFormat::AsciiString => {
                let (i, s) = context("IFDDataContents::AsciiString", take_while(|x| x != 0))(i)?;
                // TODO: check that s.len() + 1 == n_components
                let s = String::from_utf8_lossy(s);
                Ok((i, IFDDataContents::AsciiString(s.to_string())))
            }
            IFDDataFormat::UnsignedShort => {
                let (i, x) =
                    context("IFDDataContents::UnsignedShort", |i| alignment.parse_u16(i))(i)?;
                Ok((i, IFDDataContents::UnsignedShort(x)))
            }
            IFDDataFormat::UnsignedLong => {
                let (i, x) =
                    context("IFDDataContents::UnsignedLong", |i| alignment.parse_u32(i))(i)?;
                Ok((i, IFDDataContents::UnsignedLong(x)))
            }
            IFDDataFormat::UnsignedRational => {
                let (i, (num, denom)) = context(
                    "IFDDataContents::UnsignedRational",
                    tuple((|x| alignment.parse_u32(x), |x| alignment.parse_u32(x))),
                )(i)?;
                Ok((i, IFDDataContents::UnsignedRational(num, denom)))
            }
            IFDDataFormat::SignedByte => {
                let (i, x) = context("IFDDataContents::SignedByte", |i| alignment.parse_i8(i))(i)?;
                Ok((i, IFDDataContents::SignedByte(x)))
            }
            IFDDataFormat::Undefined => {
                let (i, x) = context("IFDDataContents::Undefined", take(n_components))(i)?;
                Ok((i, IFDDataContents::Undefined(x.to_vec())))
            }
            IFDDataFormat::SignedShort => {
                let (i, x) =
                    context("IFDDataContents::SignedShort", |i| alignment.parse_i16(i))(i)?;
                Ok((i, IFDDataContents::SignedShort(x)))
            }
            IFDDataFormat::SignedLong => {
                let (i, x) = context("IFDDataContents::SignedLong", |i| alignment.parse_i32(i))(i)?;
                Ok((i, IFDDataContents::SignedLong(x)))
            }
            IFDDataFormat::SignedRational => {
                let (i, (num, denom)) = context(
                    "IFDDataContents::SignedRational",
                    tuple((|x| alignment.parse_i32(x), |x| alignment.parse_i32(x))),
                )(i)?;
                Ok((i, IFDDataContents::SignedRational(num, denom)))
            }
            IFDDataFormat::SingleFloat => {
                let (i, x) =
                    context("IFDDataContents::SingleFloat", |i| alignment.parse_f32(i))(i)?;
                Ok((i, IFDDataContents::SingleFloat(x)))
            }
            IFDDataFormat::DoubleFloat => {
                let (i, x) =
                    context("IFDDataContents::DoubleFloat", |i| alignment.parse_f64(i))(i)?;
                Ok((i, IFDDataContents::DoubleFloat(x)))
            }
        }
    }

    pub fn to_le_bytes(&self) -> Vec<u8> {
        match self {
            IFDDataContents::UnsignedByte(x) => vec![*x],
            IFDDataContents::AsciiString(x) => [x.as_bytes().to_vec(), b"\x00".to_vec()].concat(),
            IFDDataContents::UnsignedShort(x) => x.to_le_bytes().to_vec(),
            IFDDataContents::UnsignedLong(x) => x.to_le_bytes().to_vec(),
            IFDDataContents::UnsignedRational(x, y) => [x.to_le_bytes(), y.to_le_bytes()].concat(),
            IFDDataContents::SignedByte(x) => x.to_le_bytes().to_vec(),
            IFDDataContents::Undefined(x) => x.to_vec(),
            IFDDataContents::SignedShort(x) => x.to_le_bytes().to_vec(),
            IFDDataContents::SignedLong(x) => x.to_le_bytes().to_vec(),
            IFDDataContents::SignedRational(x, y) => [x.to_le_bytes(), y.to_le_bytes()].concat(),
            IFDDataContents::SingleFloat(x) => x.to_le_bytes().to_vec(),
            IFDDataContents::DoubleFloat(x) => x.to_le_bytes().to_vec(),
        }
    }

    pub fn to_be_bytes(&self) -> Vec<u8> {
        match self {
            IFDDataContents::UnsignedByte(x) => vec![*x],
            IFDDataContents::AsciiString(x) => [x.as_bytes().to_vec(), b"\x00".to_vec()].concat(),
            IFDDataContents::UnsignedShort(x) => x.to_be_bytes().to_vec(),
            IFDDataContents::UnsignedLong(x) => x.to_be_bytes().to_vec(),
            IFDDataContents::UnsignedRational(x, y) => [x.to_be_bytes(), y.to_be_bytes()].concat(),
            IFDDataContents::SignedByte(x) => x.to_be_bytes().to_vec(),
            IFDDataContents::Undefined(x) => x.to_vec(),
            IFDDataContents::SignedShort(x) => x.to_be_bytes().to_vec(),
            IFDDataContents::SignedLong(x) => x.to_be_bytes().to_vec(),
            IFDDataContents::SignedRational(x, y) => [x.to_be_bytes(), y.to_be_bytes()].concat(),
            IFDDataContents::SingleFloat(x) => x.to_be_bytes().to_vec(),
            IFDDataContents::DoubleFloat(x) => x.to_be_bytes().to_vec(),
        }
    }
}

impl Serialize for IFDDataContents {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            IFDDataContents::UnsignedByte(x) => ser.serialize_u8(*x),
            IFDDataContents::AsciiString(x) => ser.serialize_str(x),
            IFDDataContents::UnsignedShort(x) => ser.serialize_u16(*x),
            IFDDataContents::UnsignedLong(x) => ser.serialize_u32(*x),
            IFDDataContents::UnsignedRational(x, y) => {
                let mut tup = ser.serialize_tuple(2)?;
                tup.serialize_element(x)?;
                tup.serialize_element(y)?;
                tup.end()
            }
            IFDDataContents::SignedByte(x) => ser.serialize_i8(*x),
            IFDDataContents::Undefined(x) => ser.serialize_bytes(x),
            IFDDataContents::SignedShort(x) => ser.serialize_i16(*x),
            IFDDataContents::SignedLong(x) => ser.serialize_i32(*x),
            IFDDataContents::SignedRational(x, y) => {
                let mut tup = ser.serialize_tuple(2)?;
                tup.serialize_element(x)?;
                tup.serialize_element(y)?;
                tup.end()
            }
            IFDDataContents::SingleFloat(x) => ser.serialize_f32(*x),
            IFDDataContents::DoubleFloat(x) => ser.serialize_f64(*x),
        }
    }
}

impl TryFrom<&IFDDataContents> for f64 {
    type Error = &'static str;

    fn try_from(data: &IFDDataContents) -> Result<Self, Self::Error> {
        match data {
            IFDDataContents::UnsignedByte(x) => Ok(*x as f64),
            IFDDataContents::UnsignedShort(x) => Ok(*x as f64),
            IFDDataContents::UnsignedLong(x) => Ok(*x as f64),
            IFDDataContents::SignedByte(x) => Ok(*x as f64),
            IFDDataContents::SignedShort(x) => Ok(*x as f64),
            IFDDataContents::SignedLong(x) => Ok(*x as f64),
            IFDDataContents::SingleFloat(x) => Ok(*x as f64),
            IFDDataContents::DoubleFloat(x) => Ok(*x as f64),
            IFDDataContents::UnsignedRational(x, y) => Ok((*x as f64) / (*y as f64)),
            IFDDataContents::SignedRational(x, y) => Ok((*x as f64) / (*y as f64)),
            _ => Err("Cannot convert data type to f64"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{TIFFByteAlignment, TIFFHeader};

    #[test]
    fn test_parse_tiff_header() {
        // Use big-endian byte alignment
        let data = b"MM\x00\x2a\x00\x00\x00\x08";
        let (_, header) = TIFFHeader::parse(data).unwrap();
        assert_eq!(header.alignment, TIFFByteAlignment::BigEndian);
        assert_eq!(header.initial_offset, 8);

        // Use little-endian byte alignment
        let data = b"II\x2a\x00\xff\x11\x00\x00";
        let (_, header) = TIFFHeader::parse(data).unwrap();
        assert_eq!(header.alignment, TIFFByteAlignment::LittleEndian);
        assert_eq!(header.initial_offset, 0x11ff);

        // The following example should result in an error since the magic bytes 0x002a are in the
        // wrong order. In this example the mode is little-endian, so they ought to be saved as
        // \x2a\x00, not the other way around.
        let data = b"II\x00\x2a\xff\x11\x00\x00";
        let result = match TIFFHeader::parse(data) {
            Err(_) => true,
            _ => false,
        };
        assert!(result);
    }

    use super::{ExifData, IFD};
    use super::{IFDDataContents, IFDDataFormat, IFDEntry, IFDTag};
    use crate::jfif::ParseableSegment;

    #[test]
    fn test_parse_exif_segment() {
        use super::ResolutionUnit;

        // Create three IFD entries:
        // - XResolution
        // - DateTime
        // - ResolutionUnits
        let entries = vec![
            IFDEntry {
                tagtype: IFDTag::XResolution,
                data_format: IFDDataFormat::UnsignedRational,
                n_components: 1,
                content: vec![IFDDataContents::UnsignedRational(72, 1)],
            },
            IFDEntry {
                tagtype: IFDTag::DateTime,
                data_format: IFDDataFormat::AsciiString,
                n_components: (b"2021:01:02 03:04:05\x00".len() as u32),
                content: vec![IFDDataContents::AsciiString(
                    "2021:01:02 03:04:05".to_string(),
                )],
            },
            IFDEntry {
                tagtype: IFDTag::ResolutionUnit,
                data_format: IFDDataFormat::UnsignedShort,
                n_components: 1,
                content: vec![IFDDataContents::UnsignedShort(ResolutionUnit::Inch as u16)],
            },
        ];

        let ifd = IFD {
            num_entries: (entries.len() as u16),
            entries: entries,
            subifds: vec![],
            offset_to_next: None,
        };

        let tiff_header = TIFFHeader {
            alignment: TIFFByteAlignment::LittleEndian,
            initial_offset: 8,
        };

        let exif = ExifData {
            data_size: 0,
            tiff_header,
            ifds: vec![ifd],
        };

        // Attempt to re-parse the segment from its bytes
        let data = exif.as_bytes();
        let exif_parsed = ExifData::parse(&data);
        assert!(exif_parsed.is_ok());
        let (_, exif_parsed) = exif_parsed.unwrap();

        // Check the TIFF headers
        assert_eq!(exif_parsed.tiff_header, tiff_header);

        // Check the IFD entries
        let entries = &exif.ifds[0].entries;
        let parsed_entries = &exif_parsed.ifds[0].entries;
        assert_eq!(parsed_entries.len(), 3);

        let parsed_tags = parsed_entries.iter().map(|x| x.tagtype).collect::<Vec<_>>();
        assert_eq!(
            parsed_tags,
            vec![
                IFDTag::XResolution,
                IFDTag::DateTime,
                IFDTag::ResolutionUnit
            ]
        );

        let content = entries.iter().map(|x| &x.content).collect::<Vec<_>>();
        let parsed_content = parsed_entries
            .iter()
            .map(|x| &x.content)
            .collect::<Vec<_>>();
        assert_eq!(parsed_content, content);

        // Ensure that we could turn the parsed Exif data back to a byte array and get the same
        // result as we did with exif.as_bytes()
        assert_eq!(data, exif_parsed.as_bytes());
    }
}
