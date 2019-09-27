#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname ${BASH_SOURCE[0]})"

CLI=$(stack exec which davl-cli)

echo 'give Bob' | $CLI Alice
echo | $CLI Bob
echo 'claim Alice' | $CLI Bob
echo | $CLI Alice
