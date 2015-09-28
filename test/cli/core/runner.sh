#!/bin/sh -eu

AMBIATA_CLI="${1:-./dist/build/ambiata/ambiata}"
RUNDIR=$(mktemp  -p /tmp -d "ambiata-cli-XXXXXX")

for f in $AMBIATA_CLI; do
    type $f > /dev/null 2>&1 || {
        echo "No $f executable found."
        exit_endtoend 1
    }
done

cleanup () {
    echo "Cleaning up"
}

trap cleanup SIGHUP SIGINT SIGQUIT SIGTERM

exit_cleanup() {
    cleanup
    exit ${1:-1}
}
