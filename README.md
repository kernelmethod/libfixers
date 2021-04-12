# libfixers

`libfixers` is a JPEG parsing library written in Rust.

The primary goal of `libfixers` is to provide Exif metadata extraction and
editing for JPEG images. You can compile it to WebAssembly to use as a purely
client-side Exif viewer/editor, or you can embed it in another library for a
wider array of functionality.

## FAQ

### Q: Why did you make this?

**A:** I wanted to learn more about Rust and WebAssembly.

### Q: Okay, but why a JPEG/Exif parser, specifically?

**A:** I've always been interested in both OSINT and online privacy, and
[Exif](https://en.wikipedia.org/wiki/Exif) scratches both of those itches for
me. The data stored in Exif is [pretty
horrifying](https://www.eff.org/deeplinks/2012/04/picture-worth-thousand-words-including-your-location),
ranging from the time and date the image was taken, to the camera model and
software version that were used, and even the GPS coordinates of where the
picture was taken. If you want a list of all of the information that can be
stored in Exif, a good place to start is the 62-page Section 4.6 of the [Exif
2.32
specification](https://web.archive.org/web/20190624045241if_/http://www.cipa.jp:80/std/documents/e/DC-008-Translation-2019-E.pdf).
While most popular social media and messaging platforms will strip out Exif
metadata nowadays, there still exist many applications where it's easy to
accidentally upload an image without removing Exif segments first.

There exist a few very good free Exif extractors out there, such as [Jeffrey's
Image Metadata Viewer](http://exif.regex.info/exif.cgi) and
[exiftool](https://exiftool.org/) (I particularly suggest the latter if you're
looking for a good command-line tool). However, I have yet to find a good Exif
editing/stripping tool that is both (a) easily useable for the layperson and (b)
does everything on the client side. The latter point is particularly important
in this context, as Exif metadata viewing and removal are often
privacy-sensitive tasks.

### Q: Why should I use this instead of a library such as [libexif](https://libexif.github.io/)?

**A:** Frankly, you probably shouldn't. libexif is an older and much more mature
project, and this is a (primarily) educational project for me. In fact, if my
main goal was to make a professional Exif editor, I probably would have started
with libexif rather than writing my own parser.

If there's an argument to be made in favor of this library, it's that it doesn't
feature any `unsafe` code, which in light of libexif's [somewhat spotty security
history](http://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=libexif) might make a
Rust-based JPEG/Exif parser more attractive. That said, this library does rely
on dependencies that use `unsafe` code, and in any case there could very well be
vulnerabilities in this library that are outside the scope of Rust's security
guarantees. Use at your own risk.

### Q: You're not really selling me on this library very well.

**A:** No, I'm not. Sorry.

### Q: Why the name `libfixers`?

**A:** Because, uh... I took the name libexif... and reversed the characters in
"exif"... and added an "rs" at the end, since it's written in Rust.

... If you can think of a better name, I am [very open to new
ideas](https://github.com/kernelmethod/libfixers/issues/new).

### Q: How do I use this crate in my own projects?

#### Compiling to WebAssembly

If you just want to compile this crate to WebAssembly, the easiest thing to do
is probably to [install
`wasm-pack`](https://rustwasm.github.io/wasm-pack/installer/) and run

```
$ wasm-pack build --target web
```

Alternatively, you can add `wasm32-unknown-unknown` as a build target and then
use `cargo build`:

```
$ rustup target add wasm32-unknown-unknown
$ cargo build --target wasm32-unknown-unknown
```

Note that `rustup target add wasm32-unknown-unknown` only needs to be done once
on your machine; i.e., if you've done this before to compile another library to
WASM, you don't need to do it again.

#### Using `libfixers` in another library

At the time of writing, this crate isn't currently being hosted on
[crates.io](https://crates.io/). If you want to use it in your own project, you
will need to use the following syntax in your `Cargo.toml`:

```
[dependencies]
...
libfixers = { git = "https://github.com/kernelmethod/libfixers" }
...
```
