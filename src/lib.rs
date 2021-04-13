#![forbid(unsafe_code)]

pub mod exif;
pub mod jfif;
mod parse;

use jfif::JFIFSegment;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

#[derive(Debug)]
pub struct JPEGFile {
    pub file_size: usize,
    pub segments: Vec<JFIFSegment>,
}

impl JPEGFile {
    // Bytes to mark the start and end of the image
    pub fn parse(i: parse::Input) -> parse::Result<Self> {
        use nom::error::context;

        let file_size = i.len();

        let mut current_input = i;
        let mut segments = Vec::new();
        while current_input.len() > 0 {
            let (i, seg) = context("JPEGFile::parse", JFIFSegment::parse)(current_input)?;
            current_input = i;
            segments.push(seg);
        }

        let file = JPEGFile {
            file_size,
            segments,
        };

        Ok((current_input, file))
    }

    /// Get all IFD entries from the file's Exif data.
    pub fn exif_metadata(&self) -> Vec<exif::IFDEntry> {
        let mut entries = Vec::new();
        for mut seg_entries in self.segments.iter().filter_map(|seg| match seg {
            JFIFSegment::ExifSegment(data) => Some(data.collect_ifd_entries()),
            _ => None,
        }) {
            entries.append(&mut seg_entries);
        }

        entries
    }

    /// Get all comments in the file.
    pub fn comments(&self) -> Vec<&String> {
        self.segments
            .iter()
            .filter_map(|seg| match seg {
                JFIFSegment::CommentSegment(data) => Some(&data.comment),
                _ => None,
            })
            .collect()
    }
}

#[cfg(test)]
mod test {
    use super::JPEGFile;
    use crate::exif::{self, IFDDataContents};

    /// Read an image from the test_images/ directory.
    fn read_img(img_name: &'static str) -> std::io::Result<Vec<u8>> {
        use std::{fs::read, path::Path};
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

    /// Get the data contents for the first occurrence of a tag in a Vec<IFDEntry>
    macro_rules! get_tag {
        ($vec: ident, $ifdtag: ident, $n: expr) => {
            $vec.iter()
                .filter_map(|x| match x.tagtype {
                    crate::exif::IFDTag::$ifdtag => Some(&x.content[$n]),
                    _ => None,
                })
                .next()
        };
    }

    /// Generic test to ensure that the file_size calculated by JPEGFile is equal to the sum of all
    /// of the segment sizes.
    fn check_segment_sizes_sum_to_file_size(file: &JPEGFile) {
        let total_seg_size = file.segments.iter().map(|s| s.data().segment_size()).sum();
        assert_eq!(file.file_size, total_seg_size);
    }

    /// Convert GPSLatitude / GPSLongitude entry contents to decimal degrees
    fn get_degrees(content: &[IFDDataContents]) -> Result<f64, &str> {
        if content.len() != 3 {
            return Err("content length != 3");
        }

        let (deg, min, sec) = (&content[0], &content[1], &content[2]);
        let (deg, min, sec) = match (deg, min, sec) {
            (
                &IFDDataContents::UnsignedRational(x1, y1),
                &IFDDataContents::UnsignedRational(x2, y2),
                &IFDDataContents::UnsignedRational(x3, y3),
            ) => (
                (x1 as f64) / (y1 as f64),
                (x2 as f64) / (y2 as f64),
                (x3 as f64) / (y3 as f64),
            ),
            _ => return Err("Invalid GPS coordinate types"),
        };

        Ok(exif::gps::degrees_to_decimal(deg, min, sec))
    }

    /// Ensure that we parse the _test/ex1.jpg image correctly
    #[test]
    fn test_parse_example_1() {
        let file = parse_img("ex1.jpg");
        assert_eq!(file.file_size, 3476);

        let exif = file.exif_metadata();
        assert_eq!(
            get_tag!(exif, DateTimeOriginal, 0),
            Some(&IFDDataContents::AsciiString(
                "2021:12:11 23:59:59".to_string()
            ))
        );

        // Check GPS coordinates
        let longitude = get_tag!(exif, GPSLongitude, ..).unwrap();
        let longitude = get_degrees(longitude).unwrap();
        assert_eq!(longitude, 0.1257400);
        let latitude = get_tag!(exif, GPSLatitude, ..).unwrap();
        let latitude = get_degrees(latitude).unwrap();
        assert_eq!(latitude, 51.5085300);

        // Check the JPEG comments
        let comments = file.comments();
        assert_eq!(comments, vec!["This is a test comment!"]);

        // Additional tests
        check_segment_sizes_sum_to_file_size(&file);
    }
}
