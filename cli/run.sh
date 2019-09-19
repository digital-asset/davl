#!/usr/bin/env bash
set -veuo pipefail

CLI=$(stack exec which davl-cli)

echo '!Bob' | $CLI

echo | $CLI Bob
#echo '!Alice' | $CLI Bob

echo | $CLI
