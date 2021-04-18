//! Provides the IFDTag enum, which specifies all possible IFD tags that can be parsed by the
//! ExifData segment parser.

use crate::{exif::TIFFByteAlignment, parse};
use derive_try_from_primitive::TryFromPrimitive;
use nom::error::context;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

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

    /// Return the memory representation of this `IFDTag` as a byte array in big-endian byte order.
    pub fn to_be_bytes(self) -> [u8; 2] {
        (self as u16).to_be_bytes()
    }

    /// Return the memory representation of this `IFDTag` as a byte array in little-endian byte
    /// order.
    pub fn to_le_bytes(self) -> [u8; 2] {
        (self as u16).to_le_bytes()
    }
}

#[cfg(test)]
mod test {
    use super::IFDTag;
    use crate::exif::TIFFByteAlignment;
    use std::convert::TryFrom;

    /// Ensure that we can convert an IFDTag to and from a u16
    #[test]
    fn test_convert_to_from_u16() {
        assert_eq!(IFDTag::try_from(0x0000), Ok(IFDTag::GPSVersionID));
        assert_eq!(IFDTag::GPSVersionID as u16, 0x0000);
        assert_eq!(IFDTag::try_from(0x011a), Ok(IFDTag::XResolution));
        assert_eq!(IFDTag::XResolution as u16, 0x011a);
    }

    /// Ensure that the parse and parse_unknown functions work correctly for all possible inputs.
    #[test]
    fn test_parse_and_parse_unknown() {
        // Since there are "only" 2^16 possible values of u16, it's not too difficult to loop over
        // all of them and run a few tests for parse and parse_unknown on all of them.
        let le_alignment = TIFFByteAlignment::LittleEndian;
        let be_alignment = TIFFByteAlignment::BigEndian;
        for i in 0..std::u16::MAX {
            let le_input = (i as u16).to_le_bytes();
            let be_input = (i as u16).to_be_bytes();
            let parse_le_result = IFDTag::parse(&le_input, le_alignment);
            let parse_be_result = IFDTag::parse(&be_input, be_alignment);
            let parse_unknown_le_result = IFDTag::parse_unknown(&le_input, le_alignment);
            let parse_unknown_be_result = IFDTag::parse_unknown(&be_input, be_alignment);

            // parse_le_result and parse_be_result should both be equal if they both return Ok. If
            // they return Err, there will be a slight difference in them since they're parsing
            // different inputs.
            match parse_le_result {
                Err(_) => assert!(parse_be_result.is_err()),
                Ok(_) => assert_eq!(parse_le_result, parse_be_result),
            };

            // The result of parse_unknown should always be Ok. In cases where there doesn't exist
            // an IFDTag for the input, we expect the result of parse_unknown to be an
            // IFDTag::Unknown.
            assert!(parse_unknown_le_result.is_ok());
            assert!(parse_unknown_be_result.is_ok());

            // In cases where parse returns Ok, the result should be the same as parse_unknown
            match parse_le_result {
                Ok(_) => {
                    assert_eq!(parse_le_result, parse_unknown_le_result);
                    assert_eq!(parse_be_result, parse_unknown_be_result);
                }
                _ => { }
            }
        }
    }
}
