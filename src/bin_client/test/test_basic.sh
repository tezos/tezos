#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

$client -w none config update

sleep 2

#tests for the rpc service raw_context
$client rpc call '/blocks/head/raw_context/version' | assert '{ "content": "616c706861" }'
$client rpc call '/blocks/head/raw_context/non-existent' | assert 'No service found at this URL'
$client rpc call '/blocks/head/raw_context/delegates/?depth=2' | assert '{ "content":
    { "ed25519":
        { "02": null, "a9": null, "c5": null, "da": null, "e7": null } } }'
$client rpc call '/blocks/head/raw_context/non-existent?depth=-1' | assert 'No service found at this URL'
$client rpc call '/blocks/head/raw_context/non-existent?depth=0' | assert 'No service found at this URL'

bake

key1=foo
key2=bar
key3=boo
key4=king
key5=queen

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

bake

$client remember program noop file:contracts/noop.tz
$client typecheck program file:contracts/noop.tz
bake_after $client originate contract noop \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/noop.tz

bake_after $client transfer 10 from bootstrap1 to noop -arg "Unit"


bake_after $client originate contract hardlimit \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/hardlimit.tz -init "3"
bake_after $client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
bake_after $client transfer 10 from bootstrap1 to hardlimit -arg "Unit"

bake_after $client originate account free_account for $key1 \
        transferring 1,000 from bootstrap1 -delegatable
$client get delegate for free_account

bake_after $client register key $key2 as delegate
bake_after $client set delegate for free_account to $key2
$client get delegate for free_account

$client get balance for bootstrap5 | assert "4,000,000 ꜩ"
bake_after $client transfer 400,000 from bootstrap5 to bootstrap1 -fee 0
bake_after $client transfer 400,000 from bootstrap1 to bootstrap5 -fee 0
$client get balance for bootstrap5 | assert "4,000,000 ꜩ"

bake_after $client activate account $key4 with king_commitment.json
bake_after $client activate account $key5 with queen_commitment.json

$client get balance for $key4 | assert "23,932,454.669,343 ꜩ"
$client get balance for $key5 | assert "72,954,577.464,032 ꜩ"

bake_after $client transfer 10 from $key4 to $key5

echo
echo End of test
echo

show_logs="no"
