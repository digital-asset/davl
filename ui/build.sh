#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

mkdir $DIR/release
echo "<html><head><title>placeholder</title></head><body><h1>Hello</h1><p>This is a placeholder.</p></body></html>" > $DIR/release/index.html
