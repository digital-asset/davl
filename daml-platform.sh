#!/bin/bash
set -euxo pipefail
set -m
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

LEDGER_HOST=localhost
LEDGER_PORT=6865
LEDGER_ID="DAVL"
NAVIGATOR_PORT=7500
JSON_API_PORT=7575

export DAML_SDK_VERSION=$(cat $DIR/SDK_VERSION)

daml sandbox --port $LEDGER_PORT --ledgerid $LEDGER_ID --wall-clock-time &
SANDBOX_PID=$!
kill_sandbox() {
  kill $SANDBOX_PID || true
}
trap kill_sandbox EXIT

sleep 1
until nc -z $LEDGER_HOST $LEDGER_PORT; do
  echo "Waiting for sandbox."
  sleep 1
done
echo "Connected to sandbox."

for DAR in $DIR/released/*.dar; do
  daml ledger upload-dar --host $LEDGER_HOST --port $LEDGER_PORT $DAR
done

daml navigator server --port $NAVIGATOR_PORT $LEDGER_HOST $LEDGER_PORT 2>&1 1> $DIR/navigtor.log &
NAVIGATOR_PID=$!

daml json-api --ledger-host $LEDGER_HOST --ledger-port $LEDGER_PORT --http-port $JSON_API_PORT 2>&1 1> $DIR/json-api.log &
JSON_API_PID=$!

kill_navigator_json_api() {
  kill $NAVIGATOR_PID || true
  kill $JSON_API_PID || true
}
trap kill_navigator_json_api EXIT

echo "Everything started. Press Ctrl-C to exit."
fg %1
