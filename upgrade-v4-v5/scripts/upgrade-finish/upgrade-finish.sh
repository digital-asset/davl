#!/usr/bin/env bash

set -euo pipefail

LEDGER_HOST=localhost
LEDGER_PORT=6865

daml build \
     -o .daml/dist/upgrade-finish.dar

daml trigger \
     --dar .daml/dist/upgrade-finish.dar \
     --trigger-name UpgradeFinish:main \
     --ledger-host $LEDGER_HOST --ledger-port $LEDGER_PORT --wall-clock-time \
     --ledger-party "Digital Asset"
