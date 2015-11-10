---
title: Downloading data from Ambiata
version: 1.0
---

You can use the `ambiata download` command to transfer data securely from
Ambiata via a download from Amazon S3.

# Required variables

 - `ORGANISATION_ID`.
   This will be provided by Ambiata to identity the current organisation.
 - `ENDPOINT_ID`.
   This will be provided by Ambiata to distinguish which files within
   the current organisation will be downloaded.
 - `DOWNLOAD_DIR`. This is the directory which will contain the files
   to be downloaded. It must be an absolute path (starts with a `/`).
   It is recommended that it is owned by same the user that
   `ambiata` process will run as. The process will require
   read, write and execute permission for the directory.

# Downloading data

Once the download process is configured and running, as/when files
are added to your configured endpoint they will automatically be
downloaded to the download directory.
