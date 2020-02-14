#!/usr/bin/env bash

set -eou pipefail

# Utility script for testing v4/v5 upgrades. Works in combination with
# 'daml_platform.sh'.
#   - Create a test v4 ledger;
#   - Issue v4/v5 upgrade proposals;
#   - Accept upgrade proposals.
#   - Upgrade vacations and requests.

test_setup="`pwd`/test-setup.json"
test_party="`pwd`/test-party.json"

yarn install && (cd admin-cli && yarn workspaces run build)
yarn run davl-admin-cli v4-init -f $test_setup
(cd upgrade-v4-v5/scripts/upgrade-init&&./upgrade-init.sh $test_party)
(cd upgrade-v4-v5/scripts/upgrade-accept&&./upgrade-accept.sh $test_party)
(cd upgrade-v4-v5/scripts/upgrade-finish&&./upgrade-finish.sh $test_party)
