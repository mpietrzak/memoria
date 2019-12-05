#!/bin/sh

cd `dirname "$0"`

RUST_BACKTRACE=1 RUST_LOG=warn nice -n19 watchexec \
    --restart \
    --signal KILL \
    'cabal run memoria'

