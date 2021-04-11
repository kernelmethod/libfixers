//! Various utilities for dealing with GPS information.

/// Convert latitude and longitude coordinates from degrees/minutes/seconds to decimal.
pub fn degrees_to_decimal(degrees: f64, minutes: f64, seconds: f64) -> f64 {
    degrees + minutes / 60. + seconds / 3600.
}
