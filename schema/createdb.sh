#!/usr/bin/env bash

set -e

DATABASE=allstreamtest

#dropdb --echo --interactive $DATABASE
dropdb --echo $DATABASE
createdb --echo $DATABASE

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

cat $DIR/schema.sql | psql $DATABASE
