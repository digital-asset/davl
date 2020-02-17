#!/usr/bin/env bash

set -eou pipefail

# Utility script for testing v4/v5 upgrades. Works in combination with
# 'daml_platform.sh'.
#   - Create a test v4 ledger;
#   - Issue v4/v5 upgrade proposals;
#   - Accept upgrade proposals.
#   - Upgrade vacations and requests.

ledger_host=localhost
ledger_port=6865
test_setup="`pwd`/test-setup.json"
test_party="`pwd`/test-party.json"

yarn install && \
 (cd admin-cli && yarn workspaces run build)
yarn run davl-admin-cli v4-init -f $test_setup
pushd upgrade-v4-v5-automation
daml build -o .daml/dist/automation.dar
daml script \
  --dar .daml/dist/automation.dar \
  --script-name Automation:init \
  --ledger-host $ledger_host --ledger-port $ledger_port --wall-clock-time \
  --input-file $test_party
daml script \
  --dar .daml/dist/automation.dar \
  --script-name Automation:accept \
  --ledger-host $ledger_host --ledger-port $ledger_port --wall-clock-time \
  --input-file $test_party
daml trigger \
  --dar .daml/dist/automation.dar \
  --trigger-name Automation:finish \
  --ledger-host $ledger_host --ledger-port $ledger_port --wall-clock-time \
  --ledger-party "Digital Asset"
popd
