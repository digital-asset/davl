#!/usr/bin/env bash

set -e

# Utility script for testing v4/v5 upgrades. Works in combination with
# 'daml_platform.sh'.
#   - Create a test v4 ledger;
#   - Issue v4/v5 upgrade proposals;
#   - Accept upgrade proposals;

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
yarn run davl-admin-cli v4-init -f $test_setup
(cd upgrade-v4-v5/scripts/upgrade-init&&./upgrade-init.sh)
(cd upgrade-v4-v5/scripts/upgrade-accept&&./upgrade-accept.sh)
