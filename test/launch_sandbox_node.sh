#!/usr/bin/env bash

source test_utils.sh

start_sandboxed_node

sleep 3

activate_alpha

alias tezos-client="${TZCLIENT} "
