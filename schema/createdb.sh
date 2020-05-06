#!/usr/bin/env bash


DATABASE=allstreamtest
dropdb --echo --interactive $DATABASE
createdb --echo $DATABASE

psql $DATABASE < 001.types.sql
psql $DATABASE < 002.events.sql
psql $DATABASE < 003.streams