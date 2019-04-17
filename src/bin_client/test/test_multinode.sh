#!/usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

parameters_file=$test_dir/protocol_parameters.json

if ! [ -f "$parameters_file" ]; then
	cat > "$parameters_file" <<EOF
{ "bootstrap_accounts":
  [
      [ "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav", "4000000000000" ],
      [ "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9", "4000000000000" ],
      [ "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV", "4000000000000" ],
      [ "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU", "4000000000000" ],
      [ "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n", "4000000000000" ]
  ],
  "commitments": [
    [ "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343" ],
    [ "btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032" ],
    [ "btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428348" ],
    [ "btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031" ],
    [ "btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550" ],
    [ "btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555" ],
    [ "btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443" ],
    [ "btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525" ],
    [ "btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693" ],
    [ "btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478" ]
  ],
  "time_between_blocks" : [ "1", "0" ],
  "blocks_per_cycle" : 128,
  "blocks_per_roll_snapshot" : 32,
  "blocks_per_voting_period" : 256,
  "preserved_cycles" : 1,
  "proof_of_work_threshold": "-1",
  "minimum_endorsements_per_priority": []
}
EOF
fi

expected_connections=4
max_peer_id=8
for i in $(seq 1 $max_peer_id); do
    echo
    echo "## Starting node $i."
    echo
    start_node $i
    echo
done

## waiting for the node to establish connections

for client in "${client_instances[@]}"; do
    echo
    echo "### $client bootstrapped"
    echo
    $client -w none config update
    $client bootstrapped
    echo
done

for admin_client in "${admin_client_instances[@]}"; do
    echo
    echo "### $admin_client network stat"
    echo
    $admin_client p2p stat
    echo
done

activate_alpha


printf "\n\n"

retry 2 15 assert_protocol "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"

$client1 bake for bootstrap1 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 2

$client2 bake for bootstrap2 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 3

$client3 bake for bootstrap3 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 4

$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp
retry 2 15 assert_propagation_level 5

endorse_hash=$($client3 endorse for bootstrap3 | extract_operation_hash)

transfer4_hash1=$($client4 transfer 500 from bootstrap1 to bootstrap3 | extract_operation_hash)
$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp
transfer4_hash2=$($client4 transfer 400 from bootstrap1 to bootstrap3 | extract_operation_hash)
$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp
transfer4_hash3=$($client4 transfer 300 from bootstrap1 to bootstrap3 | extract_operation_hash)
$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp

sleep 2

transfer3_hash1=$($client3 transfer 500 from bootstrap1 to bootstrap3 | extract_operation_hash)
$client3 bake for bootstrap4 --max-priority 512 --minimal-timestamp
transfer3_hash2=$($client3 transfer 400 from bootstrap1 to bootstrap3 | extract_operation_hash)
$client3 bake for bootstrap4 --max-priority 512 --minimal-timestamp
transfer3_hash3=$($client3 transfer 300 from bootstrap1 to bootstrap3 | extract_operation_hash)
$client3 bake for bootstrap4 --max-priority 512 --minimal-timestamp

# wait for the propagation of operations
sleep 2

assert_contains_operation() {
    hash="$1"
    # hash = '' means that the transfer didn't succeed
    if [ -z "$hash" ]; then
      exit 2
    fi
    printf "Asserting operations list contains '$hash'\n"
    for client in "${client_instances[@]}"; do
        ( $client get receipt for $hash ) || exit 2
    done
}

$client4 bake for bootstrap4 --max-priority 512 --minimal-timestamp
retry 2 15 assert_contains_operation $endorse_hash

retry 2 15 assert_contains_operation $transfer4_hash1
retry 2 15 assert_contains_operation $transfer4_hash2
retry 2 15 assert_contains_operation $transfer4_hash3

retry 2 15 assert_contains_operation $transfer3_hash1
retry 2 15 assert_contains_operation $transfer3_hash2
retry 2 15 assert_contains_operation $transfer3_hash3

echo
echo End of test
echo

show_logs="no"
