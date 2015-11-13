---
title: Uploading data to Ambiata
version: 1.0
---

You can use the `ambiata-daemon upload` command to transfer data securely to
Ambiata via an upload to Amazon S3.

# Requirements

## Required variables

 - `UPLOAD_DIR`. This is the directory which will contain the files
   to be uploaded. It must be an absolute path (starts with a `/`).
   It is recommended that it is owned by same the user that
   `ambiata-daemon upload` process will run as. The process will require
   read, write and execute permission for the directory.

## Optional variables

The default values for these variables should serve most users, but
they can be set if required.

 - `ARCHIVE_RETENTION` (default: 7). This is the number of days to wait before
   deleting files which have been successfully uploaded. Setting this
   to "0" will cause uploaded files to never be deleted.

### NTLM authentication

NTLM is not currently supported directly. The upload command can be
configured to use an NTLM-authenticating proxy via an NTLM middleware
proxy such as [Cntlm](http://cntlm.sourceforge.net/).

# Uploading data

Once the upload process is configured and running, the final step is
to add some files to the directory specified in `UPLOAD_DIR`. The
process will only upload regular files in `UPLOAD_DIR`; it won't
upload directories or symbolic links.

The process will monitor `UPLOAD_DIR` for new files, but it won't
start uploading them until they have been present in the directory for
at least 2 minutes without changing; this is to avoid uploading
partial files.
