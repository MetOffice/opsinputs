#!/bin/bash -e

# Usage:
#
#   update_gcom_sources.sh <gcom_dir>
#
# Clear the gcom/gc, gcom/gcg, gcom/include and gcom/mpl
# subdirectories. Then copy Gcom source files from <gcom_dir> (which
# should be a directory containing a working copy of the Gcom FCM
# repository) into these subdirectories.
# GCOM source available from
#   https://code.metoffice.gov.uk/trac/gcom
#

MY_DIR="$(dirname "$0")"
MY_DIR="`( cd "$MY_DIR" && pwd )`"

DEST_DIR="${MY_DIR}/gcom"
SRC_DIR="${1:?Missing Source Directory}"

echo "Clearing $DEST_DIR/gc..."
rm -rf "${DEST_DIR}/gc"

echo "Clearing ${DEST_DIR}/gcg..."
rm -rf "${DEST_DIR}/gcg"

echo "Clearing $DEST_DIR/include..."
rm -rf "${DEST_DIR}/include"

echo "Clearing ${DEST_DIR}/mpl..."
rm -rf "${DEST_DIR}/mpl"

echo "Copying files from $PWD to $DEST_DIR..."

cd "$SRC_DIR/build"
cp --parents --recursive gc "$DEST_DIR"
cp --parents --recursive gcg "$DEST_DIR"
cp --parents --recursive include "$DEST_DIR"
cp --parents --recursive mpl "$DEST_DIR"

echo "Done"
