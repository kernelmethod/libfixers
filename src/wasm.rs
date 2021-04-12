//! WebAssembly bindings for the `libfixers` crate.

use crate::{
    JPEGFile,
    exif::{self, IFDTag, IFDDataContents, IFDDataFormat},
    jfif::JFIFSegment,
};
use wasm_bindgen::prelude::*;

/// Perform some preprocessing on the IFD entries extracted by the Exif parser so that they're
/// easier to display in the browser.
fn preprocess_ifd_entries(entries: &mut [exif::IFDEntry]) {
    for e in entries.iter_mut() {
        match (e.data_format, e.tagtype) {
            // Convert XResolution and YResolution to strings of the form "X:Y"
            (_, IFDTag::XResolution) | (_, IFDTag::YResolution) => {
                e.content = e.content.iter_mut().map(|x| match x {
                    IFDDataContents::UnsignedRational(x,y) =>
                        IFDDataContents::AsciiString(format!("{}:{}", x, y)),
                    x => x.to_owned(),
                })
                .collect();
            }
            // Convert GPS coordinates to a single floating point value
            (_, IFDTag::GPSLatitude) | (_, IFDTag::GPSLongitude) => match e.content[..3] {
                [IFDDataContents::UnsignedRational(x1,y1),
                 IFDDataContents::UnsignedRational(x2,y2),
                 IFDDataContents::UnsignedRational(x3,y3)] => {
                    let degrees = (x1 as f64) / (y1 as f64);
                    let minutes = (x2 as f64) / (y2 as f64);
                    let seconds = (x3 as f64) / (y3 as f64);
                    let loc = exif::gps::degrees_to_decimal(degrees, minutes, seconds);

                    // We round to six decimal places before returning the coordinate
                    let loc = (loc * 1e6).round() / 1e6;
                    e.content = vec![IFDDataContents::DoubleFloat(loc)];
                }
                _ => { },
            }
            // Convert all other rational datatypes to floating point
            (IFDDataFormat::UnsignedRational, _) | (IFDDataFormat::SignedRational, _) => {
                e.content = e.content.iter_mut().map(|x| match x {
                    IFDDataContents::UnsignedRational(x,y) =>
                        IFDDataContents::DoubleFloat((*x as f64) / (*y as f64)),
                    IFDDataContents::SignedRational(x,y) =>
                        IFDDataContents::DoubleFloat((*x as f64) / (*y as f64)),
                    x => x.to_owned(),
                })
                .collect();
            }
            // Do nothing for remaining unmatched tags / data types
            _ => { }
        }
    }
}

/// Parse a JPEG file and return the Exif metadata stored in the image.
#[wasm_bindgen]
pub fn extract_exif_data(i: &[u8]) -> Result<JsValue, JsValue> {
    match JPEGFile::parse(i) {
        Ok((_, img)) => {
            // Try to extract Exif data, if we can find any
            let mut exif_entries = img.segments
                .iter()
                .filter_map(|s| match s {
                    JFIFSegment::APP1(data) => Some(data),
                    _ => None,
                })
                .map(|x| x.collect_ifd_entries())
                .fold(Vec::new(), |acc, x| acc.into_iter().chain(x).collect::<Vec<_>>());

            if exif_entries.len() == 0 {
                Err(JsValue::from_str("No Exif data was found in the image"))
            }
            else {
                preprocess_ifd_entries(&mut exif_entries);
                match JsValue::from_serde(&exif_entries) {
                    Ok(v) => Ok(v),
                    Err(_) => Err(JsValue::from_str("Unable to convert result to JSON!")),
                }
            }
        },
        Err(nom::Err::Failure(e)) | Err(nom::Err::Error(e)) => {
            // Convert e into a format that makes it a little easier to read
            let err = e.errors
                .iter()
                .map(|err| match err {
                    (_, nom::error::VerboseErrorKind::Context(e)) => format!("{}", e),
                    (_, e) => format!("{:?}", e),
                })
                .rev()
                .fold("JPEGFile::parse".to_string(), |acc, x| format!("{} => {}", acc, x));

            let msg = format!("Unable to parse image: Error: {}", err);
            Err(JsValue::from_str(&msg))
        }
        _ => {
            Err(JsValue::from_str("Unkown fatal error"))
        }
    }
}
