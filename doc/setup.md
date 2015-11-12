---
title: Ambiata Setup
version: 1.0
---

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

The `ambiata` command is configured using environment
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

## Optional variables

The default values for these variables should serve most users, but
they can be set if required.

 - `DEBUG` (default: unset). Setting this to "1" will enable more
   verbose logging, which may be helpful in diagnosing issues with the
   upload.

## Proxy configuration

The `ambiata` command will respect proxy configuration via
the `https_proxy` environment variable. The format of the variable is
`[protocol prefix://][username:password@]host:port`; for example,
to use a non-authenticating HTTP proxy, set
`https_proxy=http://proxy.localdomain:3128`).

### NTLM authentication

NTLM is not currently supported directly. The upload command can be
configured to use an NTLM-authenticating proxy via an NTLM middleware
proxy such as [Cntlm](http://cntlm.sourceforge.net/).
