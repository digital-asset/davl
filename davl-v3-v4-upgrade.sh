#!/usr/bin/env bash

set -euo pipefail

# Utility script for testing v3/v4 upgrades. Works in combination with
# 'daml_platform.sh'.
#   - Create a test v3 ledger;
#   - Issue v3/v4 upgrade proposals;
#   - Accept upgrade proposals;
#   - Upgrade vacations and requests.

test_setup='test-setup.json'

ledger_url='http://localhost:7575/'
ledger_id='DAVL'
application_id='DAVL'
secret='secret'
company="Digital Asset"
ledger_params="--ledger-url $ledger_url         \
               --ledger-id $ledger_id           \
               --application-id $application_id \
               --secret $secret"

yarn install && (cd admin-cli && yarn workspaces run build)
yarn run davl-admin-cli v3-init -f $test_setup
yarn run davl-admin-cli v3-v4-upgrade-init $ledger_params --company "$company"
yarn run davl-admin-cli v3-v4-upgrade-accept $ledger_params --company "$company"
yarn run davl-admin-cli v3-v4-upgrade-finish $ledger_params --company "$company"
