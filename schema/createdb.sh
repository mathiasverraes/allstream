#!/usr/bin/env bash

set -e

DATABASE=allstreamtest

#dropdb --echo --interactive $DATABASE
dropdb --echo $DATABASE
createdb --echo $DATABASE

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

cat \
  $DIR/001.config.sql \
  $DIR/002.types.sql \
  $DIR/003.events.sql \
  $DIR/004.stream_types.sql \
  $DIR/005.streams.sql \
  | psql $DATABASE
