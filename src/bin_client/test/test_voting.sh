#!/bin/bash

# Requires jq command

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

# Prepare a config with a shorter blocks_per_voting_period
tempdir=`mktemp -d`
sed -e 's/"blocks_per_voting_period" : [0-9]*/"blocks_per_voting_period" : 4/' $parameters_file > $tempdir/parameters.json
parameters_file=$tempdir/parameters.json
echo params=${parameters_file}

# Start a node
start_node 1
activate_alpha

echo Alpha activated

function get_ballot_list() {
    $client rpc get /chains/main/blocks/head/votes/ballot_list
}
function get_ballots() {
    $client rpc get /chains/main/blocks/head/votes/ballots
}
function get_current_period_kind() {
    $client rpc get /chains/main/blocks/head/votes/current_period_kind
}
function get_current_proposal() {
    $client rpc get /chains/main/blocks/head/votes/current_proposal
}
function get_current_quorum() {
    $client rpc get /chains/main/blocks/head/votes/current_quorum
}
function get_listings() {
    $client rpc get /chains/main/blocks/head/votes/listings
}
function get_proposals() {
    $client rpc get /chains/main/blocks/head/votes/proposals
}
function get_period_position() {
    #TODO why offset 1?
    $client rpc get /chains/main/blocks/head/helpers/current_level?offset=1 | jq .voting_period_position
}

$client show voting period

[ `get_period_position` = '1' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
echo Checking the bug of the empty listing in the first voting period...
[ `get_listings` = '[]' ] \
    || { echo "empty listings bug was fixed?!" ; exit 1 ; }

bake # pos=2

[ `get_period_position` = '2' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }

bake # pos=3
bake # new period, pos=0

echo 'Checking the current period = proposal with non empty listings'
[ `get_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ "`get_listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }

# Prepare 3 different protocol sources

echo 'Injecting protocols...'

cp -r proto_test_injection $tempdir/proto1
proto1=`$admin_client inject protocol $tempdir/proto1 | sed -E 's/Injected protocol (.*) successfully/\1/'`

cp -r proto_test_injection $tempdir/proto2
echo '(* 2 *)' >> $tempdir/proto2/main.ml
proto2=`$admin_client inject protocol $tempdir/proto2 | sed -E 's/Injected protocol (.*) successfully/\1/'`

cp -r proto_test_injection $tempdir/proto3
echo '(* 3 *)' >> $tempdir/proto3/main.ml
proto3=`$admin_client inject protocol $tempdir/proto3 | sed -E 's/Injected protocol (.*) successfully/\1/'`

proto=($proto1 $proto2 $proto3)
printf 'New injected protocol: %s\n' "${proto[@]}"

[[ "${#proto[@]}" == "3" ]] \
    || { echo "Invalid number of injected protocols" ; exit 1 ; }

# Proposals

[ `get_proposals` == '[]' ] \
    || { echo "strange proposals" ; exit 1 ; }

echo 'Proposal voting...'

$client show voting period

$client submit proposals for bootstrap1 ${proto[0]}
$client submit proposals for bootstrap2 ${proto[0]} ${proto[1]}
$client submit proposals for bootstrap3 ${proto[1]}
$client submit proposals for bootstrap4 ${proto[2]}

bake

$client show voting period

[ "`get_proposals`" != '[]' ] \
    || { echo "strange proposals" ; exit 1 ; }

bake # pos=2

echo 'Breaking the tie'

$client submit proposals for bootstrap4 ${proto[1]} # To make ${proto[1]} win
$client show voting period

bake # pos=3
bake # new period! pos=0

echo The phase must be testing_vote...

[ `get_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `get_current_period_kind` = '"testing_vote"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`get_listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `get_current_proposal` = '"'${proto[1]}'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }

echo Ballots 1
$client submit ballot for bootstrap1 ${proto[1]} yay
$client submit ballot for bootstrap2 ${proto[1]} yay
$client submit ballot for bootstrap3 ${proto[1]} yay
$client submit ballot for bootstrap4 ${proto[1]} yay

bake # pos=1

# They cannot change their mind.
echo "Ballots 2 (should fail)"
$client submit ballot for bootstrap1 ${proto[1]} yay \
    && { echo "submit ballot cannot be called twice" ; exit 1 ; }

bake # pos=2
bake # pos=3

$client show voting period

bake # new period pos=0

echo Testing vote should be done
[ `get_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `get_current_period_kind` = '"testing"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`get_listings`" = '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `get_current_proposal` = '"'${proto[1]}'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `get_ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }

bake # pos=1
bake # pos=2
bake # pos=3
bake # new period pos=0

echo Testing should be done
[ `get_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `get_current_period_kind` = '"promotion_vote"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`get_listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `get_current_proposal` = '"'${proto[1]}'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `get_ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }

$client submit ballot for bootstrap1 ${proto[1]} yay
$client submit ballot for bootstrap2 ${proto[1]} yay
$client submit ballot for bootstrap3 ${proto[1]} yay
$client submit ballot for bootstrap4 ${proto[1]} nay # not to promote

bake # pos=1
bake # pos=2
bake # pos=3

$client show voting period

bake # new period pos=0

echo 'Promotion vote should be over now negatively'
[ `get_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `get_current_period_kind` = '"proposal"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`get_listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `get_current_proposal` = 'null' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `get_ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }
