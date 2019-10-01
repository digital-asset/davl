#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname ${BASH_SOURCE[0]})"

CLI=$(stack exec which davl-cli)

echo 'give Bob' | $CLI Alice

echo 'history' | $CLI Bob
echo 'summary' | $CLI Bob
echo 'claim Alice' | $CLI Bob

echo 'history' | $CLI Alice
echo 'summary' | $CLI Alice

echo 'request 100' | $CLI Bob
echo 'summary' | $CLI Bob

echo 'summary' | $CLI Alice
echo 'pending' | $CLI Alice
echo 'deny 1 work-harder' | $CLI Alice
echo 'summary' | $CLI Alice

echo 'summary' | $CLI Bob
