#! /bin/sh -eu

# This is an example run script for the Ambiata upload command, which
# sets the configuration variables and executes the upload process. It
# does not perform any kind of process supervision, and so it is
# recommended to use a process manager instead of this script if possible.

# Replace these values with the relevant ones for your organisation.
export AMBIATA_API_KEY=6a7c5f66-d2d3-4b86-96ed-1770c1abb942
export UPLOAD_DIR=/home/upload/data/

# Use an HTTP proxy which requires authentication.
export https_proxy=http://username:password@proxy.localdomain:3128

exec /opt/ambiata/bin/ambiata-daemon upload
