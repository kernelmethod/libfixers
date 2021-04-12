#![forbid(unsafe_code)]

pub mod exif;
pub mod jfif;
mod parse;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

use crate::jfif::JFIFSegment;

#[derive(Debug)]
pub struct JPEGFile {
    pub segments: Vec<JFIFSegment>,
}

impl JPEGFile {
    // Bytes to mark the start and end of the image
    pub fn parse(i: parse::Input) -> parse::Result<Self> {
        use nom::error::context;

        let mut current_input = i;
        let mut segments = Vec::new();
        while current_input.len() > 0 {
            // Try to parse another segment of the image
            let (i, seg) = context("Segment", JFIFSegment::parse)(current_input)?;
            segments.push(seg);
            current_input = i;
        }

        Ok((current_input, JPEGFile { segments }))
    }
}
