#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/lib/test_lib.inc.sh

start_node 1
activate_alpha

key1=foo
key2=bar

$client gen keys $key1
$client gen keys $key2

$client list known identities

$client transfer 1000 from bootstrap1 to $key1
$client transfer 2000 from bootstrap1 to $key2

$client get balance for $key1 | assert "1,000.00 ꜩ"
$client get balance for $key2 | assert "2,000.00 ꜩ"

$client transfer 1000 from $key2 to $key1

$client get balance for $key1 | assert "2,000.00 ꜩ"
$client get balance for $key2 | assert "999.95 ꜩ"

# Should fail
# $client transfer 999.95 from $key2 to $key1

# wait for the delay between two block
sleep 1

$client mine for bootstrap1 -max-priority 512

$client remember program noop file:contracts/noop.tz
$client typecheck program noop
$client originate contract noop \
        for $key1 transferring 1000 from bootstrap1 \
        running noop
$client transfer 10 from bootstrap1 to noop -arg "Unit"

$client originate contract hardlimit \
        for $key1 transferring 1000 from bootstrap1 \
        running file:contracts/hardlimit.tz -init "3"
$client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
$client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
# $client transfer 10 from bootstrap1 to hardlimit -arg "unit" # should fail

$client originate free account free_account for $key1
$client get delegate for free_account
$client set delegate for free_account to $key2
$client get delegate for free_account

$client get balance for bootstrap5 | assert "4,000,000.00 ꜩ"
$client transfer 4000000.00 from bootstrap5 to bootstrap1 -fee 0
$client transfer 4000000.00 from bootstrap1 to bootstrap5 -fee 0
$client get balance for bootstrap5 | assert "4,000,000.00 ꜩ"

echo
echo End of test
echo

show_logs="no"
