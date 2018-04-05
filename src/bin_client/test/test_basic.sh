#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

sleep 2

#tests for the rpc service raw_context
$client rpc call '/blocks/head/raw_context/version' | assert '{ "content": "616c706861" }'
$client rpc call '/blocks/head/raw_context/non-existent' | assert 'No service found at this URL'
$client rpc call '/blocks/head/raw_context/delegates/?depth=2' | assert '{ "content":
    { "ed25519":
        { "02": null, "a9": null, "c5": null, "da": null, "e7": null } } }'
$client rpc call '/blocks/head/raw_context/non-existent?depth=-1' | assert 'No service found at this URL'
$client rpc call '/blocks/head/raw_context/non-existent?depth=0' | assert 'No service found at this URL'


$client bake for bootstrap1 -max-priority 512
sleep 1

key1=foo
key2=bar
key3=boo

$client gen keys $key1
$client gen keys $key2 --sig secp256k1
$client gen keys $key3 --sig ed25519

$client list known identities
$client get balance for bootstrap1

bake_after $client transfer 1,000 from bootstrap1 to $key1
bake_after $client transfer 2,000 from bootstrap1 to $key2
bake_after $client transfer 3,000 from bootstrap1 to $key3

$client get balance for $key1 | assert "1,000 ꜩ"
$client get balance for $key2 | assert "2,000 ꜩ"
$client get balance for $key3 | assert "3,000 ꜩ"

bake_after $client transfer 1,000 from $key2 to $key1 -fee 0
$client get balance for $key1 | assert "2,000 ꜩ"
$client get balance for $key2 | assert "1,000 ꜩ"

bake_after $client transfer 1,000 from $key1 to $key2
$client get balance for $key1 | assert "999.95 ꜩ"
$client get balance for $key2 | assert "2,000 ꜩ"

# Should fail
# $client transfer 999.95 from $key2 to $key1

# wait for the delay between two block
sleep 1

$client bake for bootstrap1 -max-priority 512

$client remember program noop file:contracts/noop.tz
$client typecheck program noop
$client originate contract noop \
        for $key1 transferring 1,000 from bootstrap1 \
        running noop
sleep 1
$client bake for bootstrap1 -max-priority 512
$client transfer 10 from bootstrap1 to noop -arg "Unit"
sleep 1
$client bake for bootstrap1 -max-priority 512


$client originate contract hardlimit \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/hardlimit.tz -init "3"
sleep 1
$client bake for bootstrap1 -max-priority 512
$client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
sleep 1
$client bake for bootstrap1 -max-priority 512
$client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
# $client transfer 10 from bootstrap1 to hardlimit -arg "unit" # should fail
sleep 1
$client bake for bootstrap1 -max-priority 512

$client originate account free_account for $key1 \
        transferring 1,000 from bootstrap1 -delegatable
sleep 1
$client bake for bootstrap1 -max-priority 512
$client get delegate for free_account
sleep 1
$client bake for bootstrap1 -max-priority 512
$client register key $key2 as delegate
sleep 1
$client bake for bootstrap1 -max-priority 512
$client set delegate for free_account to $key2
sleep 1
$client bake for bootstrap1 -max-priority 512
$client get delegate for free_account

$client get balance for bootstrap5 | assert "4,000,000 ꜩ"
$client transfer 400,000 from bootstrap5 to bootstrap1 -fee 0
sleep 1
$client bake for bootstrap1 -max-priority 512
$client transfer 400,000 from bootstrap1 to bootstrap5 -fee 0
sleep 1
$client bake for bootstrap1 -max-priority 512
$client get balance for bootstrap5 | assert "4,000,000 ꜩ"

echo
echo End of test
echo

show_logs="no"
