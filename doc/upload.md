---
title: Uploading data to Ambiata
version: 1.0
---

You can use the `ambiata upload` command to transfer data securely to
Ambiata via an upload to Amazon S3.

# Requirements

## Firewall

Outbound access via TCP port 443 (HTTPS) to [Amazon S3 and Amazon
EC2](https://docs.aws.amazon.com/general/latest/gr/aws-ip-ranges.html)
in the `ap-southeast-2` region is required. Inbound access is not required.

## Server

The `ambiata` command is distributed as a binary which should run on
any modern x86-64 based Linux system.

The command runs as a long-lived daemon process. It is strongly
recommended that it be run under a process supervisor like `Upstart`,
`systemd` or `daemontools` which will start the command on boot and
restart it if it crashes.

It will print log messages to standard output. If run under a process
manager, logs will generally be redirected to the system logging
facility (e.g., `journald` or `syslog`).

# Configuration

The `ambiata upload` command is configured using environment
variables; these can be set either by the process supervisor or via a
"run script" which configures and runs the command. Example
configuration files for some process managers and a sample "run
script" are available in the release tarball under the
`share/doc/examples` directory.

If you use any of the configuration examples, you will need to update
the environment variables to replace the example values with real ones
as described below. Additionally, the configuration examples assume
that the `ambiata` tarball was extracted in the `/opt` directory on
the server.

## Required variables

 - `AMBIATA_API_KEY`. This is a token used for authentication to
   Ambiata's API. It is unique per-customer, and must not be shared.
 - `UPLOAD_DIR`. This is the directory which will contain the files
   to be uploaded. It must be an absolute path (starts with a `/`).
   It is recommended that it is owned by same the user that
   `ambiata upload` process will run as. The process will require
   read, write and execute permission for the directory.

## Optional variables

The default values for these variables should serve most users, but
they can be set if required.

 - `ARCHIVE_RETENTION` (default: 7). This is the number of days to wait before
   deleting files which have been successfully uploaded. Setting this
   to "0" will cause uploaded files to never be deleted.
 - `DEBUG` (default: unset). Setting this to "1" will enable more
   verbose logging, which may be helpful in diagnosing issues with the
   upload.

## Proxy configuration

The `ambiata upload` command will respect proxy configuration via
the `https_proxy` environment variable. The format of the variable is
`[protocol prefix://][username:password@]host:port`; for example, 
to use a non-authenticating HTTP proxy, set
`https_proxy=http://proxy.localdomain:3128`).

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
