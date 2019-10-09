#!/usr/bin/env bash
set -euo pipefail

# barebones regression test for CLI

cd "$(dirname ${BASH_SOURCE[0]})"

stack build

pkill -f sandbox.jar || true

cd testcases
rm -f *.log

echo start sandbox
daml sandbox > /dev/null &
# Register sandbox killing as a trap so it executes regardless of whether this
# script succeeds
SANDBOX_PID=$!
kill_sandbox() {
  echo kill sandbox
  kill -9 $SANDBOX_PID
}
trap kill_sandbox EXIT
sleep 5 # TODO: do better!

echo deploy
(cd ../../project/V1; daml deploy) > /dev/null

echo cli, case1
./case1.sh > case1.log

# The log files would be much cleaner if the CLI really was a CLI,
# instead of a console which displays colour etc
echo compare
diff case1.log{.expected,}
