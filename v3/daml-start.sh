#!/bin/sh
daml start \
  --open-browser no \
  --sandbox-option --wall-clock-time \
  --sandbox-option --ledgerid \
  --sandbox-option "davl" \
  # --sandbox-option --sql-backend-jdbcurl \
  # --sandbox-option "jdbc:postgresql://localhost/davl?user=davl&password=davl" \
  $*
