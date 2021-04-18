use libfixers::{jfif, JPEGFile, parse, version};
use std::{env, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = env::args()
        .nth(1)
        .expect(format!("Usage: ./{} <file>", version::name()).as_str());
    let data = fs::read(&input_path)?;
    
    let img = match JPEGFile::parse(&data) {
        Ok((_, img)) => img,
        Err(e) => panic!("{}", parse::pretty_error_message(&data, e)),
    };

    println!("Image segments:");
    let mut total_offset = 0;
    for seg in &img.segments {
        let data = seg.data();
        let seg_end = total_offset + data.segment_size();
        println!("- {:?}: magic = {:?}, start = 0x{:x?}, end = 0x{:x?}", seg, data.marker(), total_offset, seg_end);
        total_offset = seg_end;
    }
    println!();

    let exif = img.segments
        .iter()
        .filter_map(|s| match s {
            jfif::JFIFSegment::ExifSegment(data) => Some(data),
            _ => None,
        })
        .next();

    match exif {
        Some(data) => {
            println!("Found the following Exif data:");
            println!();
            println!("{:30} {:20} {}", "Tag type", "Data type", "Data");
            println!("--------------------------------------------------------------------------");
            for entry in data.collect_ifd_entries() {
                let tagtype = format!("{:?}", entry.tagtype);
                let data_format = format!("{:?}", entry.data_format);
                let content = if entry.content.len() == 1 {
                    format!("{:?}", entry.content[0])
                } else {
                    format!("{:?}", entry.content)
                };
                println!("{:30} {:20} {}", tagtype, data_format, content);
            }
        }
        None => {
            println!("No Exif data was found.");
        }
    }

    Ok(())
}
