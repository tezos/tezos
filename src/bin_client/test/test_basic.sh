#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

sleep 2

#tests for the rpc service raw_context
$client rpc call '/blocks/head/raw_context/version' | assert '{ "content": "67656e65736973" }'
$client rpc call '/blocks/head/raw_context/non-existent' | assert 'No service found at this URL'
$client rpc call '/blocks/head/raw_context?depth=2' | assert '{ "content":
    { "genesis_key":
        "68b4bf512517497dbd944de6825ab0a0fed7ff51bdd6b77596a19cc9175ddd55",
      "v1": { "sandboxed": null }, "version": "67656e65736973" } }'
$client rpc call '/blocks/head/raw_context/non-existent?depth=-1' | assert 'No service found at this URL'
$client rpc call '/blocks/head/raw_context/non-existent?depth=0' | assert 'No service found at this URL'


$client bake for bootstrap1 -max-priority 512

key1=foo
key2=bar

$client gen keys $key1
$client gen keys $key2

$client list known identities
$client get balance for bootstrap1

$client transfer 1,000 from bootstrap1 to $key1
$client transfer 2,000 from bootstrap1 to $key2

$client get balance for $key1 | assert "1,000 ꜩ"
$client get balance for $key2 | assert "2,000 ꜩ"

$client transfer 1,000 from $key2 to $key1

$client get balance for $key1 | assert "2,000 ꜩ"
$client get balance for $key2 | assert "999.95 ꜩ"

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
$client transfer 10 from bootstrap1 to noop -arg "Unit"

$client originate contract hardlimit \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/hardlimit.tz -init "3"
$client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
$client transfer 10 from bootstrap1 to hardlimit -arg "Unit"
# $client transfer 10 from bootstrap1 to hardlimit -arg "unit" # should fail

$client get balance for bootstrap5 | assert "4,000,000 ꜩ"
$client transfer 400,000 from bootstrap5 to bootstrap1 -fee 0
$client transfer 400,000 from bootstrap1 to bootstrap5 -fee 0
$client get balance for bootstrap5 | assert "4,000,000 ꜩ"

echo
echo End of test
echo

show_logs="no"
