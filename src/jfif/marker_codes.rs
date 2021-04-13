//! Defines the `JFIFMarkerCode` type, which enumerates all of the possible JFIF markers that are
//! allowed by the standard.

use crate::impl_parse_for_enum;
use serde::{Deserialize, Serialize};
use std::convert::{From, TryFrom};

/// Marker codes for JFIF segments. See ISO/IEC 10918-1: 1993(E), p. 36 for more information.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum JFIFMarkerCode {
    // Start Of Frame markers, non-differential, Huffman coding
    SOF0, // 0xFFC0: Baseline DCT
    SOF1, // 0xFFC1: Extended sequential DCT
    SOF2, // 0xFFC2: Progressive DCT
    SOF3, // 0xFFC3: Lossless (sequential)

    // Start Of Frame markers, differential, Huffman coding
    SOF5, // 0xFFC5: Differential sequential DCT
    SOF6, // 0xFFC6: Differential progressive DCT
    SOF7, // 0xFFC7: Differential lossless (sequential)

    // Start Of Frame markers, non-differential, arithmetic coding
    JPG,   // 0xFFC8: Reserved for JPEG extensions
    SOF9,  // 0xFFC9: Extended sequential DCT
    SOF10, // 0xFFCA: Progressive DCT
    SOF11, // 0xFFCB: Lossless (sequential)

    // Start Of Frame markers, differential, arithmetic encoding
    SOF13, // 0xFFCD: Differential sequential DCT
    SOF14, // 0xFFCE: Differential progressive DCT
    SOF15, // 0xFFCF: Differential lossless (sequential)

    // Huffman table specification
    DHT, // 0xFFC4: Define Huffman table(s)

    // Arithmetic conditioning specification
    DAC, // 0xFFCC: Define arithmetic coding conditioning(s)

    // Restart interval termination
    RSTm(u8), // 0xFFD0 - 0xFFD7: Restart with modulo 8 count "m"

    // Other markers
    SOI,      // 0xFFD8: Start of image
    EOI,      // 0xFFD9: End of image
    SOS,      // 0xFFDA: Start of scan
    DQT,      // 0xFFDB: Define quantization table(s)
    DNL,      // 0xFFDC: Define number of lines
    DRI,      // 0xFFDD: Define restart interval
    DHP,      // 0xFFDE: Define hierarchical progression
    EXP,      // 0xFFDF: Expand reference component(s)
    APPm(u8), // 0xFFE0 - 0xFFEF: Reserved for application segments
    JPGm(u8), // 0xFFF0 - 0xFFFD: Reserved for JPEG extensions
    COM,      // 0xFFFE: Comment

    // Reserved markers
    TEM,     // 0xFF01: For temporary private use in arithmetic coding
    RES(u8), // 0xFF02-FFBF: Reserved
}

// Define JFIFMarkerCode::parse(i: parse::Input) -> parse::Result by parsing the marker code from a
// 16-bit integer.
impl_parse_for_enum!(JFIFMarkerCode, be_u16);

impl JFIFMarkerCode {
    pub fn description(&self) -> &'static str {
        match self {
            JFIFMarkerCode::SOF0 => "Start Of Frame, non-differential, Huffman coding: Baseline DCT",
            JFIFMarkerCode::SOF1 => "Start Of Frame, non-differential, Huffman coding: Extended sequential DCT",
            JFIFMarkerCode::SOF2 => "Start Of Frame, non-differential, Huffman coding: Progressive DCT",
            JFIFMarkerCode::SOF3 => "Start Of Frame, non-differential, Huffman coding: Lossless (sequential)",

            JFIFMarkerCode::SOF5 => "Start Of Frame, differential, Huffman coding: Differential sequential DCT",
            JFIFMarkerCode::SOF6 => "Start Of Frame, differential, Huffman coding: Differential progressive DCT",
            JFIFMarkerCode::SOF7 => "Start Of Frame, differential, Huffman coding: Differential lossless (sequential)",

            JFIFMarkerCode::JPG | JFIFMarkerCode::JPGm(_) => "Reserved for JPEG extensions",

            JFIFMarkerCode::SOF9  => "Start Of Frame, non-differential, arithmetic coding: Extended sequential DCT",
            JFIFMarkerCode::SOF10 => "Start Of Frame, non-differential, arithmetic coding: Progressive DCT",
            JFIFMarkerCode::SOF11 => "Start Of Frame, non-differential, arithmetic coding: Lossless (sequential)",

            JFIFMarkerCode::SOF13 => "Start Of Frame, differential, arithmetic coding: Differential sequential DCT",
            JFIFMarkerCode::SOF14 => "Start Of Frame, differential, arithmetic coding: Differential progressive DCT",
            JFIFMarkerCode::SOF15 => "Start Of Frame, differential, arithmetic coding: Differential lossless (sequential)",

            JFIFMarkerCode::DHT => "Define Huffman Table(s)",
            JFIFMarkerCode::DAC => "Define Arithmetic Coding conditioning(s)",
            JFIFMarkerCode::RSTm(_) => "Restart with modulo 8 count \"m\"",

            JFIFMarkerCode::SOI => "Start Of Image",
            JFIFMarkerCode::EOI => "End Of Image",
            JFIFMarkerCode::SOS => "Start Of Scan",
            JFIFMarkerCode::DQT => "Define Quantization Table(s)",
            JFIFMarkerCode::DNL => "Define Number of Lines",
            JFIFMarkerCode::DRI => "Define Restart Interval",
            JFIFMarkerCode::DHP => "Define Hierarchical Progression",
            JFIFMarkerCode::EXP => "Expand Reference Component(s)",
            JFIFMarkerCode::APPm(_) => "Reserved for application segments",
            JFIFMarkerCode::COM => "Comment",

            JFIFMarkerCode::TEM => "For temporary private use in arithmetic coding",
            JFIFMarkerCode::RES(_) => "Reserved",
        }
    }

    pub fn as_bytes(self) -> [u8; 2] {
        u16::from(self).to_be_bytes()
    }
}

impl TryFrom<u16> for JFIFMarkerCode {
    type Error = &'static str;

    fn try_from(code: u16) -> Result<Self, Self::Error> {
        let result = match code {
            // Start by trying to match the input code against the marker codes that are restricted
            // to a single possible value
            0xFF01 => JFIFMarkerCode::TEM,

            0xFFC0 => JFIFMarkerCode::SOF0,
            0xFFC1 => JFIFMarkerCode::SOF1,
            0xFFC2 => JFIFMarkerCode::SOF2,
            0xFFC3 => JFIFMarkerCode::SOF3,
            0xFFC4 => JFIFMarkerCode::DHT,
            0xFFC5 => JFIFMarkerCode::SOF5,
            0xFFC6 => JFIFMarkerCode::SOF6,
            0xFFC7 => JFIFMarkerCode::SOF7,
            0xFFC8 => JFIFMarkerCode::JPG,
            0xFFC9 => JFIFMarkerCode::SOF9,
            0xFFCA => JFIFMarkerCode::SOF10,
            0xFFCB => JFIFMarkerCode::SOF11,
            0xFFCC => JFIFMarkerCode::DAC,
            0xFFCD => JFIFMarkerCode::SOF13,
            0xFFCE => JFIFMarkerCode::SOF14,
            0xFFCF => JFIFMarkerCode::SOF15,

            0xFFD8 => JFIFMarkerCode::SOI,
            0xFFD9 => JFIFMarkerCode::EOI,
            0xFFDA => JFIFMarkerCode::SOS,
            0xFFDB => JFIFMarkerCode::DQT,
            0xFFDC => JFIFMarkerCode::DNL,
            0xFFDD => JFIFMarkerCode::DRI,
            0xFFDE => JFIFMarkerCode::DHP,
            0xFFDF => JFIFMarkerCode::EXP,
            0xFFFE => JFIFMarkerCode::COM,

            _ => {
                // If we didn't get a match on any of the other tags, we resort to checking the
                // ranges defined for the remaining tags.
                //
                // Note that we use the (a..=b) syntax to define a `RangeInclusive` rather than
                // `Range`, the difference being that (a..=b).contains(x) checks a <= x <= b, while
                // (a..b).contains(x) checks a <= x < b.
                if (0xFF02..=0xFFBF).contains(&code) {
                    // Store the full last byte in the enum
                    JFIFMarkerCode::RES((code & 0x00FF) as u8)
                } else if (0xFFD0..=0xFFD7).contains(&code) {
                    // Store the last four bits in the enum so that we have a number relative to
                    // 0xFFD0. E.g., 0xFFD4 will be mapped to JFIFMarkerCode::RSTm(0x04)
                    JFIFMarkerCode::RSTm((code & 0x000F) as u8)
                } else if (0xFFE0..=0xFFEF).contains(&code) {
                    // Store the last four bits in the enum
                    JFIFMarkerCode::APPm((code & 0x000F) as u8)
                } else if (0xFFF0..=0xFFFD).contains(&code) {
                    // Store the last four bits in the enum
                    JFIFMarkerCode::JPGm((code & 0x000F) as u8)
                } else {
                    // We can only arrive at this point if the first byte is not 0xFF, since there
                    // exists a code for every other possible value
                    return Err("Invalid marker code (code must begin with 0xFF!)");
                }
            }
        };

        Ok(result)
    }
}

impl From<JFIFMarkerCode> for u16 {
    fn from(marker: JFIFMarkerCode) -> Self {
        match marker {
            JFIFMarkerCode::TEM => 0xFF01,

            JFIFMarkerCode::SOF0 => 0xFFC0,
            JFIFMarkerCode::SOF1 => 0xFFC1,
            JFIFMarkerCode::SOF2 => 0xFFC2,
            JFIFMarkerCode::SOF3 => 0xFFC3,
            JFIFMarkerCode::DHT => 0xFFC4,
            JFIFMarkerCode::SOF5 => 0xFFC5,
            JFIFMarkerCode::SOF6 => 0xFFC6,
            JFIFMarkerCode::SOF7 => 0xFFC7,
            JFIFMarkerCode::JPG => 0xFFC8,
            JFIFMarkerCode::SOF9 => 0xFFC9,
            JFIFMarkerCode::SOF10 => 0xFFCA,
            JFIFMarkerCode::SOF11 => 0xFFCB,
            JFIFMarkerCode::DAC => 0xFFCC,
            JFIFMarkerCode::SOF13 => 0xFFCD,
            JFIFMarkerCode::SOF14 => 0xFFCE,
            JFIFMarkerCode::SOF15 => 0xFFCF,

            JFIFMarkerCode::SOI => 0xFFD8,
            JFIFMarkerCode::EOI => 0xFFD9,
            JFIFMarkerCode::SOS => 0xFFDA,
            JFIFMarkerCode::DQT => 0xFFDB,
            JFIFMarkerCode::DNL => 0xFFDC,
            JFIFMarkerCode::DRI => 0xFFDD,
            JFIFMarkerCode::DHP => 0xFFDE,
            JFIFMarkerCode::EXP => 0xFFDF,
            JFIFMarkerCode::COM => 0xFFFE,

            JFIFMarkerCode::RES(code) => 0xFF00 | (code as u16),
            JFIFMarkerCode::RSTm(code) => 0xFFD0 | (code as u16),
            JFIFMarkerCode::APPm(code) => 0xFFE0 | (code as u16),
            JFIFMarkerCode::JPGm(code) => 0xFFF0 | (code as u16),
        }
    }
}

#[cfg(test)]
mod test {
    use super::JFIFMarkerCode;
    use std::convert::{Into, TryFrom};

    #[test]
    fn test_to_and_from_u16() {
        // There are only 65,536 16-bit integers, so it doesn't take too much effort test test
        // creating a JFIFMarkerCode from all of them.

        for code in 0..=0xFFFF {
            let code = code as u16;

            // There only exists a matching JFIFMarkerCode for codes that are between 0xFF01 and
            // 0xFFFE (inclusive). JFIFMarkerCode::try_from(code) should succeed for these values
            // and fail for all others.
            let in_valid_range = (0xFF01..=0xFFFE).contains(&code);
            let jfif_code = match JFIFMarkerCode::try_from(code) {
                Ok(c) => {
                    assert!(in_valid_range);
                    c
                }
                Err(_) => {
                    assert!(!in_valid_range);
                    continue;
                }
            };

            // If `JFIFMarkerCode::try_from` was successful, then `into` should return the original
            // code.
            assert_eq!(code, jfif_code.into());
        }
    }

    #[test]
    fn test_markers_that_store_last_byte_of_code() {
        // Four of the markers -- RES, RSTm, APPm, JPGm -- all store part of the last byte of the
        // input code in them. For RSTm, APPm, and JPGm, only the last four bits are stored. The
        // following tests ensure that the code is stored correctly for each of these markers.
        for code in 0xFF02..=0xFFBF {
            let code = code as u16;
            let last_byte = (code & 0xFF) as u8;
            assert_eq!(
                JFIFMarkerCode::try_from(code),
                Ok(JFIFMarkerCode::RES(last_byte))
            );
        }
    }
}
