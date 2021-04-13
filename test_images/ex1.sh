#!/bin/bash

# Script for generating first test image.

INFILE="$1"
OUTDIR="."
OUTFILE="ex1.jpg"

mkdir -p "${OUTDIR}"
rm -f "${OUTDIR}/{${INFILE},${OUTFILE}}"

./exiftool \
    `# Remove existing metadata` \
    -all= \
    `# Add new metadata` \
    -Comment="This is a test comment!" \
    -Keywords="exif" -Keywords="test" \
    -DateTime="2021:01:02 03:04:05" \
    -DateTimeOriginal="2021-12-11T23:59:59" \
    `# Add GPS info corresponding to London, UK` \
    -gps:GPSLatitude=51.5085300 \
    -gps:GPSLongitude=-0.1257400 \
    `# Write results to $OUTDIR/` \
    -o "$OUTDIR" \
    "$INFILE"

# Rename file to $OUTDIR/$OUTFILE to avoid conflicts with any other test scripts
mv "${OUTDIR}/${INFILE}" "${OUTDIR}/${OUTFILE}"

# View data in modified image
./exiftool "${OUTDIR}/${OUTFILE}"
