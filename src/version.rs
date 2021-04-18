//! Version and build.

/// Return the program name.
pub const fn name() -> &'static str {
    match option_env!("PROGRAM_NAME") {
        Some(s) => s,
        None => "libfixers",
    }
}

/// Return the program version.
pub const fn version() -> Option<&'static str> {
    option_env!("CARGO_PKG_VERSION")
}
