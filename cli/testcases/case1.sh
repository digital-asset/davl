#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname ${BASH_SOURCE[0]})"

CLI=$(stack exec which davl-cli)

# Alice gifts 2 days of holiday allocation to each of Bob and Charlie
echo 'give Bob' | $CLI Alice
echo 'give Bob' | $CLI Alice
echo 'give Charlie' | $CLI Alice
echo 'give Charlie' | $CLI Alice
echo 'summary' | $CLI Alice

# Bob claims one day of his allocation
echo 'history' | $CLI Bob
echo 'summary' | $CLI Bob
echo 'claim Alice' | $CLI Bob

# What does Alice see?
echo 'history' | $CLI Alice
echo 'summary' | $CLI Alice

# Bob make a holiday request using his single allocation
echo 'request 100' | $CLI Bob
echo 'summary' | $CLI Bob

# Alice sees the request, and denies it
echo 'summary' | $CLI Alice
echo 'pending' | $CLI Alice
echo 'deny 1 not-that-day-please' | $CLI Alice
echo 'summary' | $CLI Alice

# Bob sees the denial, any make a request for a different day
echo 'summary' | $CLI Bob
echo 'denials' | $CLI Bob
echo 'request 200' | $CLI Bob
echo 'summary' | $CLI Bob

# Alice sees the new request and accepts it
echo 'summary' | $CLI Alice
echo 'pending' | $CLI Alice
echo 'approve 1' | $CLI Alice
echo 'summary' | $CLI Alice
echo 'vacations' | $CLI Bob

# Bob sees his scheduled vacation day
echo 'summary' | $CLI Bob
echo 'vacations' | $CLI Bob
