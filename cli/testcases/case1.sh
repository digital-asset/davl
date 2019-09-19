#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname ${BASH_SOURCE[0]})"

CLI=$(stack exec which davl-cli)

echo '!Bob' | $CLI
echo | $CLI Bob
echo '!Alice' | $CLI Bob
echo | $CLI
