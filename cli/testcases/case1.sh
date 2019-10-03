#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname ${BASH_SOURCE[0]})"

CLI=$(stack exec which davl-cli)

# Alice gifts 2 days of holiday allocation to each of Bob and Charlie
sleep 1; echo 'give Bob' | $CLI Alice
sleep 1; echo 'give Bob' | $CLI Alice
sleep 1; echo 'give Charlie' | $CLI Alice
sleep 1; echo 'give Charlie' | $CLI Alice
sleep 1; echo 'summary' | $CLI Alice

# Bob claims one day of his allocation
sleep 1; echo 'history' | $CLI Bob
sleep 1; echo 'summary' | $CLI Bob
sleep 1; echo 'claim Alice' | $CLI Bob

# What does Alice see?
sleep 1; echo 'history' | $CLI Alice
sleep 1; echo 'summary' | $CLI Alice

# Bob make a holiday request using his single allocation
sleep 1; echo 'request 100' | $CLI Bob
sleep 1; echo 'summary' | $CLI Bob

# Alice sees the request, and denies it
sleep 1; echo 'summary' | $CLI Alice
sleep 1; echo 'pending' | $CLI Alice
sleep 1; echo 'deny 1 not-that-day-please' | $CLI Alice
sleep 1; echo 'summary' | $CLI Alice

# Bob sees the denial, any make a request for a different day
sleep 1; echo 'summary' | $CLI Bob
sleep 1; echo 'denials' | $CLI Bob
sleep 1; echo 'request 200' | $CLI Bob
sleep 1; echo 'summary' | $CLI Bob

# Alice sees the new request and accepts it
sleep 1; echo 'summary' | $CLI Alice
sleep 1; echo 'pending' | $CLI Alice
sleep 1; echo 'approve 1' | $CLI Alice
sleep 1; echo 'summary' | $CLI Alice
sleep 1; echo 'vacations' | $CLI Bob

# Bob sees his scheduled vacation day
sleep 1; echo 'summary' | $CLI Bob
sleep 1; echo 'vacations' | $CLI Bob
