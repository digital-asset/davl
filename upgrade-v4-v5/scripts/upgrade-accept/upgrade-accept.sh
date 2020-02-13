#!/usr/bin/env bash

set -euo pipefail

LEDGER_HOST=localhost
LEDGER_PORT=6865

daml build \
     -o .daml/dist/upgrade-accept.dar

daml script \
     --dar .daml/dist/upgrade-accept.dar \
     --script-name UpgradeAccept:main \
     --ledger-host $LEDGER_HOST --ledger-port $LEDGER_PORT --wall-clock-time \
     --input-file $1
