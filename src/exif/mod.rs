//! Parsing for the Exif data structure.

pub mod gps;

use crate::{
    impl_parse_for_enum,
    jfif::{JFIFMarkerCode, ParseableSegment},
    parse,
};
use derive_try_from_primitive::TryFromPrimitive;
use nom::{bytes::complete::tag, error::context, sequence::tuple, Offset};
use serde::{ser::SerializeTuple, Deserialize, Serialize, Serializer};
use std::convert::TryFrom;

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
#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IFDEntry {
    pub tagtype: IFDTag,
    pub data_format: IFDDataFormat,
    pub n_components: u32,
    pub content_offset: u32,
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
        let (i, content_offset, content) = if content_size <= 4 {
            let input_after_data = &i[4..];
            let (_, content) = parser(&i)?;
            let offset = (original_input.offset(input_after_data) + 4) as u32;
            (input_after_data, offset, content)
        } else {
            let (i, offset) = context("IFD entry offset", |x| alignment.parse_u32(x))(i)?;
            let (_, content) = parser(&original_input[(offset as usize)..])?;
            (i, offset, content)
        };

        Ok((
            i,
            IFDEntry {
                tagtype,
                data_format,
                n_components,
                content_offset,
                content,
            },
        ))
    }
}

#[derive(Debug, Clone, Copy, TryFromPrimitive, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u16)]
pub enum IFDTag {
    // Catch-all tag for unknown entry types
    Unknown = 0xffff,

    // GPS tags
    // Taken from https://www.exiftool.org/TagNames/GPS.html
    GPSVersionID = 0x0000,
    GPSLatitudeRef = 0x0001,
    GPSLatitude = 0x0002,
    GPSLongitudeRef = 0x0003,
    GPSLongitude = 0x0004,
    GPSAltitudeRef = 0x0005,
    GPSAltitude = 0x0006,
    GPSTimeStamp = 0x0007,
    GPSSatellites = 0x0008,
    GPSStatus = 0x0009,
    GPSMeasureMode = 0x000a,
    GPSDOP = 0x000b,
    GPSSpeedRef = 0x000c,
    GPSSpeed = 0x000d,
    GPSTrackRef = 0x000e,
    GPSTrack = 0x000f,
    GPSImgDirectionRef = 0x0010,
    GPSImgDirection = 0x0011,
    GPSMapDatum = 0x0012,
    GPSDestLatitudeRef = 0x0013,
    GPSDestLatitude = 0x0014,
    GPSDestLongitudeRef = 0x0015,
    GPSDestLongitude = 0x0016,
    GPSDestBearingRef = 0x0017,
    GPSDestBearing = 0x0018,
    GPSDestDistanceRef = 0x0019,
    GPSDestDistance = 0x001a,
    GPSProcessingMethod = 0x001b,
    GPSAreaInformation = 0x001c,
    GPSDateStamp = 0x001d,
    GPSDifferential = 0x001e,
    GPSHPositioningError = 0x001f,

    // Taken from https://www.exiftool.org/TagNames/EXIF.html
    ImageWidth = 0x0100,
    ImageLength = 0x0101,
    BitsPerSample = 0x0102,
    Compression = 0x0103,
    PhotometricInterpretation = 0x0106,
    ImageDescription = 0x010e,
    Make = 0x010f,
    Model = 0x0110,
    StripOffsets = 0x0111,
    Orientation = 0x0112,
    SamplesPerPixel = 0x0115,
    RowsPerStrip = 0x0116,
    StripByteCounts = 0x0117,
    XResolution = 0x011a,
    YResolution = 0x011b,
    PlanarConfiguration = 0x011c,
    ResolutionUnit = 0x0128,
    Software = 0x0131,
    DateTime = 0x0132,
    WhitePoint = 0x013e,
    PrimaryChromaticities = 0x013f,
    JpegIFByteCount = 0x0202,
    YCbCrCoefficients = 0x0211,
    YCbCrSubSampling = 0x0212,
    YCbCrPositioning = 0x0213,
    ReferenceBlackWhite = 0x0214,

    Copyright = 0x8298,
    ExposureTime = 0x829a,
    FNumber = 0x829d,
    ExifOffset = 0x8769,
    ExposureProgram = 0x8822,
    GPSInfo = 0x8825,
    ISOSpeedRatings = 0x8827,

    ExifVersion = 0x9000,
    DateTimeOriginal = 0x9003,
    DateTimeDigitized = 0x9004,
    ComponentConfiguration = 0x9101,
    CompressedBitsPerPixel = 0x9201,
    ApertureValue = 0x9202,
    BrightnessValue = 0x9203,
    ExposureBiasValue = 0x9204,
    MaxApertureValue = 0x9205,
    SubjectDistance = 0x9206,
    MeteringMode = 0x9207,
    LightSource = 0x9208,
    Flash = 0x9209,
    FocalLength = 0x920a,
    Noise = 0x920d,
    ImageNumber = 0x9211,
    SecurityClassification = 0x9212,
    ImageHistory = 0x9213,
    SubjectArea = 0x9214,
    ExposureIndex = 0x9215,
    TIFFEPStandardID = 0x9216,
    MakerNote = 0x927c,
    UserComment = 0x9286,
    SubSecTime = 0x9290,
    SubSecTimeOriginal = 0x9291,
    SubSecTimeDigitized = 0x9292,

    FlashPixVersion = 0xa000,
    ColorSpace = 0xa001,
    ExifImageWidth = 0xa002,
    ExifImageHeight = 0xa003,
    RelatedSoundFile = 0xa004,
    ExifInteroperabilityOffset = 0xa005,
    FlashEnergy = 0xa20b,
    SpatialFrequencyResponse = 0xa20c,
    FocalPlaneXResolution = 0xa20e,
    FocalPlaneYResolution = 0xa20f,
    FocalPlaneResolutionUnit = 0xa210,
    SubjectLocation = 0xa214,
    ExposureIndexExifIFD = 0xa215,
    SensingMethod = 0xa217,
    FileSource = 0xa300,
    SceneType = 0xa301,
    CFAPattern = 0xa302,
    CustomRendered = 0xa401,
    ExposureMode = 0xa402,
    WhiteBalance = 0xa403,
    DigitalZoomRatio = 0xa404,
    FocalLengthIn35mmFormat = 0xa405,
    SceneCaptureType = 0xa406,
    GainControl = 0xa407,
    Contrast = 0xa408,
    Saturation = 0xa409,
    Sharpness = 0xa40a,
    DeviceSettingDescription = 0xa40b,
    SubjectDistanceRange = 0xa40c,
}

impl IFDTag {
    /// Parse an IFD tag.
    pub fn parse(i: parse::Input, alignment: TIFFByteAlignment) -> parse::Result<Self> {
        use nom::{combinator::map_res, error::ErrorKind};

        let parser = map_res(
            |x| alignment.parse_u16(x),
            |x| Self::try_from(x).map_err(|_| ErrorKind::Alt),
        );

        context("IFDTag", parser)(i)
    }

    /// Parse an IFD tag. If the tag type is unknown, replace it with `IFDTag::Unknown`.
    pub fn parse_unknown(i: parse::Input, alignment: TIFFByteAlignment) -> parse::Result<Self> {
        let (i, tag) = context("IFDTag", |x| alignment.parse_u16(x))(i)?;
        match Self::try_from(tag) {
            Ok(tag) => Ok((i, tag)),
            Err(_) => Ok((i, IFDTag::Unknown)),
        }
    }
}

/// Represents all of the possible values that a ResolutionUnit IFD field can adopt.
#[derive(Debug, Clone, Copy, Eq, PartialEq, TryFromPrimitive)]
#[repr(u16)]
pub enum ResolutionUnitEnum {
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
        use nom::bytes::complete::take_while;
        let n_components = n_components as usize;

        match format {
            IFDDataFormat::UnsignedByte => {
                let (i, x) = alignment.parse_u8(i)?;
                Ok((i, IFDDataContents::UnsignedByte(x)))
            }
            IFDDataFormat::AsciiString => {
                let (i, s) = take_while(|x| x != 0)(i)?;
                // TODO: check that s.len() + 1 == n_components
                let s = String::from_utf8_lossy(s);
                Ok((i, IFDDataContents::AsciiString(s.to_string())))
            }
            IFDDataFormat::UnsignedShort => {
                let (i, x) = alignment.parse_u16(i)?;
                Ok((i, IFDDataContents::UnsignedShort(x)))
            }
            IFDDataFormat::UnsignedLong => {
                let (i, x) = alignment.parse_u32(i)?;
                Ok((i, IFDDataContents::UnsignedLong(x)))
            }
            IFDDataFormat::UnsignedRational => {
                let (i, (num, denom)) =
                    tuple((|x| alignment.parse_u32(x), |x| alignment.parse_u32(x)))(i)?;
                Ok((i, IFDDataContents::UnsignedRational(num, denom)))
            }
            IFDDataFormat::SignedByte => {
                let (i, x) = alignment.parse_i8(i)?;
                Ok((i, IFDDataContents::SignedByte(x)))
            }
            IFDDataFormat::Undefined => {
                let x = i[..n_components].to_vec();
                Ok((&i[n_components..], IFDDataContents::Undefined(x)))
            }
            IFDDataFormat::SignedShort => {
                let (i, x) = alignment.parse_i16(i)?;
                Ok((i, IFDDataContents::SignedShort(x)))
            }
            IFDDataFormat::SignedLong => {
                let (i, x) = alignment.parse_i32(i)?;
                Ok((i, IFDDataContents::SignedLong(x)))
            }
            IFDDataFormat::SignedRational => {
                let (i, (num, denom)) =
                    tuple((|x| alignment.parse_i32(x), |x| alignment.parse_i32(x)))(i)?;
                Ok((i, IFDDataContents::SignedRational(num, denom)))
            }
            IFDDataFormat::SingleFloat => {
                let (i, x) = alignment.parse_f32(i)?;
                Ok((i, IFDDataContents::SingleFloat(x)))
            }
            IFDDataFormat::DoubleFloat => {
                let (i, x) = alignment.parse_f64(i)?;
                Ok((i, IFDDataContents::DoubleFloat(x)))
            }
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
            IFDDataContents::UnsignedRational(x,y) => Ok((*x as f64) / (*y as f64)),
            IFDDataContents::SignedRational(x,y) => Ok((*x as f64) / (*y as f64)),
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
}
