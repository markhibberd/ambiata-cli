#!/bin/sh -eux

AMBIATA_CLI="${1:-./dist/build/ambiata/ambiata}"
TATOOINE="./test-helpers/tatooine"
MOUNTEBANK="./test-helpers/mountebank-tatooine"
RUNDIR=$(mktemp  -p /tmp -d "ambiata-cli-XXXXXX")

MOUNTEBANK_PIDFILE=${RUNDIR}/$(basename $MOUNTEBANK).pid
TATOOINE_PIDFILE=${RUNDIR}/$(basename $TATOOINE).pid
MOUNTEBANK_PORTFILE=${RUNDIR}/$(basename $MOUNTEBANK).port
TATOOINE_PORTFILE=${RUNDIR}/$(basename $TATOOINE).port

cleanup_endtoend () {
    kill -15 $(cat $MOUNTEBANK_PIDFILE) || true
    kill -15 $(cat $TATOOINE_PIDFILE) || true
}

exit_endtoend () {
    cleanup_endtoend
    exit ${1:-1}
}

for f in $AMBIATA_CLI $TATOOINE $MOUNTEBANK; do
    type $f > /dev/null 2>&1 || {
        echo "No $f executable found."
        exit 1
    }
done

# FIXME(sio): do this less-horribly.
bound_to () {
    pid=$1
    port=$(netstat -ntlp 2>/dev/null |\
        grep -E "\s$1/" |\
        perl -pe 's/\s+/\t/g' |\
        cut -f4 |\
        cut -d: -f2)
    if [[ -z $port ]]; then
        echo "Unable to find port for PID $pid, bailing out." >2
        exit_endtoend 1
    fi
    echo -n $port
}

setup_endtoend () {
    # Run mock proxy
    eval "( MOUNTEBANK_PORT=0 ${MOUNTEBANK} ) &"
    mb_pid=$!
    sleep 1 # this is unpleasant
    echo -n $mb_pid > $MOUNTEBANK_PIDFILE
    mb_port=$(bound_to $mb_pid)
    echo -n $mb_port > $MOUNTEBANK_PORTFILE
    tatooine_port=$RANDOM

    # Run tatooine server
    eval "( TATOOINE_PORT=${tatooine_port} TATOOINE_UPLOAD_BUCKET=$RANDOM TATOOINE_DOWNLOAD_BUCKET=$RANDOM ${TATOOINE} ) &"
    tatooine_pid=$!
    sleep 1
    echo -n $tatooine_pid > $TATOOINE_PIDFILE
    echo -n $tatooine_port > $TATOOINE_PORTFILE
}

banner () {
    echo
    echo == "$*" ==
    echo
}

trap cleanup_endtoend EXIT SIGHUP SIGINT SIGQUIT SIGTERM

setup_endtoend
