#! /bin/sh -eu

# This is an example run script for the Ambiata upload command, which
# sets the configuration variables and executes the upload process. It
# does not perform any kind of process supervision, and so it is
# recommended to use a process manager instead of this script if possible.

# Replace these values with the relevant ones for your organisation.
export AMBIATA_API_KEY=18c57b5cde3dc531dbfa19e781f24605e
export UPLOAD_DIR=/home/upload/data/

# If required, use an HTTP proxy which requires authentication.
# export https_proxy=http://username:password@proxy.localdomain:3128

exec /opt/ambiata/bin/ambiata-daemon upload
