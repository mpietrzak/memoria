#!/bin/sh

RUST_BACKTRACE=1 RUST_LOG=warn nice watchexec \
    --restart \
    --clear \
    --signal KILL \
    'cabal run memoria'

