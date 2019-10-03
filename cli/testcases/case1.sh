#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname ${BASH_SOURCE[0]})"

CLI=$(stack exec which davl-cli)

as () {
    party=$1
    cmd=$2
    echo As $party: $cmd
    (sleep 2; echo "$cmd"; sleep 2) | $CLI $party
    echo; echo --
}

# Alice gifts 2 days of holiday allocation to each of Bob and Charlie
as Alice 'give Bob'
as Alice 'give Bob'
as Alice 'give Charlie'
as Alice 'give Charlie'
as Alice 'summary'

# Bob claims one day of his allocation
as Bob 'history'
as Bob 'summary'
as Bob 'claim Alice'

# What does Alice see?
as Alice 'history'
as Alice 'summary'

# Bob make a holiday request using his single allocation
as Bob 'request 100'
as Bob 'summary'

# Alice sees the request, and denies it
as Alice 'summary'
as Alice 'pending'
as Alice 'deny 1 not-that-day-please'
as Alice 'summary'

# Bob sees the denial, any make a request for a different day
as Bob 'summary'
as Bob 'denials'
as Bob 'request 200'
as Bob 'summary'

# Alice sees the new request and accepts it
as Alice 'summary'
as Alice 'pending'
as Alice 'approve 1'
as Alice 'summary'
as Alice 'vacations'

# Bob sees his scheduled vacation day
as Bob 'summary'
as Bob 'vacations'
