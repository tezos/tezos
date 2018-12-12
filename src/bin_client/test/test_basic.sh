#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

$client -w none config update

sleep 2

#tests for the rpc service raw_context
$client rpc get '/chains/main/blocks/head/context/raw/bytes/non-existent' | assert 'No service found at this URL'
$client rpc get '/chains/main/blocks/head/context/raw/bytes/delegates/?depth=3' | assert '{ "ed25519":
    { "02": { "29": null }, "a9": { "ce": null }, "c5": { "5c": null },
      "da": { "c9": null }, "e7": { "67": null } } }'
$client rpc get '/chains/main/blocks/head/context/raw/bytes/non-existent?depth=-1' | assert 'Command failed : Extraction depth -1 is invalid'
$client rpc get '/chains/main/blocks/head/context/raw/bytes/non-existent?depth=0' | assert 'No service found at this URL'

bake

key1=foo
key2=bar
key3=boo
key4=king
key5=queen
# key6=p256

$client gen keys $key1
$client gen keys $key2 --sig secp256k1
$client gen keys $key3 --sig ed25519
# $client gen keys $key6 --sig p256

$client list known addresses
$client get balance for bootstrap1

bake_after $client transfer 1,000 from bootstrap1 to $key1 --burn-cap 0.257
bake_after $client transfer 2,000 from bootstrap1 to $key2 --burn-cap 0.257
bake_after $client transfer 3,000 from bootstrap1 to $key3 --burn-cap 0.257
# bake_after $client transfer 4,000 from bootstrap1 to $key6

$client get balance for $key1 | assert "1000 ꜩ"
$client get balance for $key2 | assert "2000 ꜩ"
$client get balance for $key3 | assert "3000 ꜩ"

bake_after $client transfer 1,000 from $key2 to $key1 --fee 0 --force-low-fee
$client get balance for $key1 | assert "2000 ꜩ"
$client get balance for $key2 | assert "1000 ꜩ"

bake_after $client transfer 1,000 from $key1 to $key2 --fee 0.05
$client get balance for $key1 | assert "999.95 ꜩ"
$client get balance for $key2 | assert "2000 ꜩ"

# Should fail
# $client transfer 999.95 from $key2 to $key1

bake

$client remember script noop file:contracts/opcodes/noop.tz
$client typecheck script file:contracts/opcodes/noop.tz
bake_after $client originate contract noop \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/opcodes/noop.tz --burn-cap 0.295

bake_after $client transfer 10 from bootstrap1 to noop --arg "Unit"


bake_after $client originate contract hardlimit \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/mini_scenarios/hardlimit.tz --init "3" --burn-cap 0.341
bake_after $client transfer 10 from bootstrap1 to hardlimit --arg "Unit"
bake_after $client transfer 10 from bootstrap1 to hardlimit --arg "Unit"

bake_after $client originate account free_account for $key1 \
        transferring 1,000 from bootstrap1 --delegatable --burn-cap 0.257
$client get delegate for free_account

bake_after $client register key $key2 as delegate
bake_after $client set delegate for free_account to $key2
$client get delegate for free_account

$client get balance for bootstrap5 | assert "4000000 ꜩ"
bake_after $client transfer 400,000 from bootstrap5 to bootstrap1 --fee 0 --force-low-fee
bake_after $client transfer 400,000 from bootstrap1 to bootstrap5 --fee 0 --force-low-fee
$client get balance for bootstrap5 | assert "4000000 ꜩ"

bake_after $client activate account $key4 with king_commitment.json
bake_after $client activate account $key5 with queen_commitment.json

$client get balance for $key4 | assert "23932454.669343 ꜩ"
$client get balance for $key5 | assert "72954577.464032 ꜩ"

bake_after $client transfer 10 from $key4 to $key5

echo
echo End of test
echo

show_logs="no"
