#! /usr/bin/env bash

set -e

#test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source src/bin_client/test/test_lib.inc.sh "$@"

empty_mempool='{ "applied": [], "refused": [], "branch_refused": [], "branch_delayed": [],
  "unprocessed": [] }'

rpc_not_found='Unregistred error:
  { "kind": "generic",
    "error": "Prevalidator is not running, cannot inject the operation" }
Fatal error: transfer simulation failed.'

prev_not_running='[]'

assert_mempool_not_empty() {
    local expected=$empty_mempool
    local result="$(cat)"
    if [ "${result}" = "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Should be different than: \"${expected}\""
        exit 2
    fi
}

assert_mempool_empty() {
    local expected=$empty_mempool
    local result="$(cat)"
    if [ "${result}" != "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Expected: \"${expected}\""
        exit 2
    fi
}

assert_rpc_not_exist() {
    local expected=$rpc_not_found
    local result="$(cat)"
    if [ "${result}" != "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Expected: \"${expected}\""
        exit 2
    fi
}

assert_rpc_exists() {
    local expected=$rpc_not_found
    local result="$(cat)"
    if [ "${result}" = "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Should be different than: \"${expected}\""
        exit 2
    fi
}

assert_prev_not_running() {
    local expected=$prev_not_running
    local result="$(cat)"
    if [ "${result}" != "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Expected: \"${expected}\""
        exit 2
    fi
}

assert_prev_running() {
    local expected=$prev_not_running
    local result="$(cat)"
    if [ "${result}" = "${expected}" ]; then
        echo "Unexpected result: \"${result}\""
        echo "Should be different than: \"${expected}\""
        exit 2
    fi
}

echo
echo Starting 1st node
echo

start_node 1

echo
echo Starting 2nd node
echo

start_node 3

echo
echo Starting 3rd node
echo

start_node 9 --disable-mempool

echo
echo Activating Alpha protocol
echo

activate_alpha

sleep 1

echo
$client3 rpc call '/network/peers' with '{}'
echo

#tests for the prevalidator state
$client1 rpc call '/workers/prevalidators' | assert_prev_running
echo 1st node prevalidator is running
$client3 rpc call '/workers/prevalidators' | assert_prev_running
echo 2nd node prevalidator is running
$client9 rpc call '/workers/prevalidators' | assert_prev_not_running
echo 3dr node prevalidator is not running

#checks that mempools are empty
$client1 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 1st node mempool is empty

$client3 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 2nd node mempool is empty

$client9 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 3rd node mempool is empty

#inject a transaction through the 1st node
$client1 transfer 1,000 from bootstrap1 to bootstrap2 &

sleep 3

#checks that mempools are not empty for enable-mempool nodes and
#empty for the disable-mempool node
$client1 rpc call '/mempool/pending_operations' | assert_mempool_not_empty
echo 1st node mempool is not empty

$client3 rpc call '/mempool/pending_operations' | assert_mempool_not_empty
echo 2nd node mempool is not empty

$client9 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 3rd node mempool is empty

$client1 bake for bootstrap1

sleep 1

#checks that all mempool are empty after baking a block which contains the
#transaction
$client1 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 1st node mempool is empty

$client3 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 2nd node mempool is empty

$client9 rpc call '/mempool/pending_operations' | assert_mempool_empty
echo 3rd node mempool is empty

#checks that trying to inject an operation through a disable-mempool node fails
{
    $client9 transfer 2,000 from bootstrap2 to bootstrap3 2> /dev/null &&
    if [ $? = 0 ]; then
        echo "This should have failed... exit code:" $?
        exit 2
    fi
} ||

echo
echo End of test
echo

show_logs="no"
