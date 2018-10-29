#!/bin/bash

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

$client -w none config update

bake

key1=foo
key2=bar

$client gen keys $key1
$client gen keys $key2

printf "\n\n"

# Assert all contracts typecheck
if [ ! $NO_TYPECHECK ] ; then
    for contract in `ls $contract_attic_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# Map block on lists
assert_storage $contract_attic_dir/list_map_block.tz '{0}' '{}' '{}'
assert_storage $contract_attic_dir/list_map_block.tz '{0}' '{ 1 ; 1 ; 1 ; 1 }' '{ 1 ; 2 ; 3 ; 4 }'
assert_storage $contract_attic_dir/list_map_block.tz '{0}' '{ 1 ; 2 ; 3 ; 0 }' '{ 1 ; 3 ; 5 ; 3 }'

# Reverse a list
assert_storage $contract_attic_dir/reverse.tz '{""}' '{}' '{}'
assert_storage $contract_attic_dir/reverse.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'


# Reverse using LOOP_LEFT
assert_storage $contract_attic_dir/loop_left.tz '{""}' '{}' '{}'
assert_storage $contract_attic_dir/loop_left.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'


# TODO [a-k] contracts in attic

# List of transactions
# TODO


# Queue
# TODO

# Reduce map
# TODO

# Reentrancy
# TODO

# Spawn identities
# TODO

printf "\nEnd of test\n"

show_logs="no"
