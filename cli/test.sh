#!/usr/bin/env bash
set -euo pipefail

# barebones regression test for CLI

cd "$(dirname ${BASH_SOURCE[0]})"

pkill -f sandbox.jar || true

cd testcases
rm -f *.log

echo start sandbox
(cd ../..; daml sandbox &) > /dev/null
sleep 5 # TODO: do better!

echo deploy
(cd ../..; daml deploy) > /dev/null

echo cli, case1
./case1.sh > case1.log

# The log files would be much cleaner if the CLI really was a CLI,
# instead of a console which displays colour etc
echo compare
cmp case1.log{.expected,}

echo kill sandbox
pkill -f sandbox.jar
