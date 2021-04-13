#![forbid(unsafe_code)]

pub mod exif;
pub mod jfif;
mod parse;

use jfif::ParseableSegment;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

#[derive(Debug)]
pub struct JPEGFile {
    pub file_size: usize,
}

impl JPEGFile {
    // Bytes to mark the start and end of the image
    pub fn parse(i: parse::Input) -> parse::Result<Self> {
        use nom::error::context;

        let file_size = i.len();

        let mut current_input = i;
        while current_input.len() > 0 {
            if exif::ExifData::can_parse_segment(current_input) {
                let (i, _seg) = context("Exif segment", exif::ExifData::parse)(current_input)?;
                current_input = i;
            }
            else {
                let (i, _seg) = context("Unknown JFIF segment", jfif::UnknownJFIFSegment::parse)(current_input)?;
                current_input = i;

            }
        }

        let file = JPEGFile {
            file_size,
        };

        Ok((current_input, file))
    }
}

#[cfg(test)]
mod test {
    use super::JPEGFile;

    /// Read an image from the test_images/ directory.
    fn read_img(img_name: &'static str) -> std::io::Result<Vec<u8>> {
        use std::{fs::read,path::Path};
        let path = Path::new("test_images").join(img_name);
        let data = read(path)?;
        Ok(data)
    }

    /// Parse an image from the test_images/ directory as a JPEG file. In the case of an error,
    /// this function panics with a neatly-formatted error message.
    fn parse_img(img_name: &'static str) -> JPEGFile {
        use crate::parse;

        let data = match read_img(img_name) {
            Ok(data) => data,
            Err(e) => panic!("Unable to read file: {:?}", e),
        };

        match JPEGFile::parse(&data) {
            Ok((_, jpeg)) => jpeg,
            Err(e) => panic!("{}", parse::pretty_error_message(&data, e)),
        }
    }

    /// Ensure that we parse the _test/ex1.jpg image correctly
    #[test]
    fn test_parse_example_1() {
        let file = parse_img("ex1.jpg");
        assert_eq!(file.file_size, 3476);
    }
}


