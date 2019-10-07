#!/usr/bin/env bash
set -euo pipefail

# setup ledger to a known state at start of demo
# assumes a sandbox is running

cd "$(dirname ${BASH_SOURCE[0]})"

host='localhost'
port=6556

stack build
CLI="$(stack exec which davl-cli) --host $host --port $port"

(cd ../project/V1; daml deploy --host $host --port $port)

sleep 2; $CLI Alice <<EOF
give Bob
give Bob
give Bob
give Charlie
give Charlie
give Charlie
EOF

sleep 2; $CLI Bob <<EOF
claim Alice
EOF
sleep 2; $CLI Bob <<EOF
request 17749
EOF
sleep 2; $CLI Alice <<EOF
approve 1
EOF

sleep 2; $CLI Bob <<EOF
claim Alice
EOF
sleep 2; $CLI Bob <<EOF
request 17750
EOF
sleep 2; $CLI Alice <<EOF
approve 1
EOF

sleep 2; $CLI Bob <<EOF
claim Alice
EOF
sleep 2; $CLI Bob <<EOF
request 17751
EOF
sleep 2; $CLI Alice <<EOF
approve 1
EOF


sleep 2; $CLI Charlie <<EOF
claim Alice
EOF
sleep 2; $CLI Charlie <<EOF
request 17756
EOF
sleep 2; $CLI Alice <<EOF
approve 1
EOF

sleep 2; $CLI Charlie <<EOF
claim Alice
EOF
sleep 2; $CLI Charlie <<EOF
request 17757
EOF
sleep 2; $CLI Alice <<EOF
approve 1
EOF

sleep 2; $CLI Charlie <<EOF
claim Alice
EOF
sleep 2; $CLI Charlie <<EOF
request 17758
EOF
sleep 2; $CLI Alice <<EOF
approve 1
EOF


sleep 2; $CLI Alice <<EOF
summary
vacations
EOF
