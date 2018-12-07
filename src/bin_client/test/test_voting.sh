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

res=`$client show votes`
echo $res

[ `echo $res | jq .voting_period_position` = '1' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '3' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
echo Checking the bug of the empty listing in the first voting period...
[ `echo $res | jq .listings` = '[]' ] \
    || { echo "empty listings bug was fixed?!" ; exit 1 ; }

bake # pos=2

res=`$client show votes`
[ `echo $res | jq .voting_period_position` = '2' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '2' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }

bake # pos=3
bake # new period, pos=0

echo 'Checking the current period = proposal with non empty listings'
res=`$client show votes`
[ `echo $res | jq .voting_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '4' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
[ "`echo $res | jq .listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }

# Prepare 3 different protocol sources

echo 'Injecting protocols...'

cp -r demo $tempdir/proto1
$admin_client inject protocol $tempdir/proto1

cp -r demo $tempdir/proto2
echo '(* 2 *)' >> $tempdir/proto2/main.ml
$admin_client inject protocol $tempdir/proto2

cp -r demo $tempdir/proto3
echo '(* 3 *)' >> $tempdir/proto3/main.ml
$admin_client inject protocol $tempdir/proto3

proto_str=`$admin_client list protocols | head -3` # assuming new protocols listed first
echo New protocols: $proto_str
proto=($proto_str)

# Proposals

echo 'Proposal voting...'

$client submit proposals for bootstrap1 ${proto[0]}
$client submit proposals for bootstrap2 ${proto[0]} ${proto[1]}
$client submit proposals for bootstrap3 ${proto[1]}
$client submit proposals for bootstrap4 ${proto[2]}

bake

res=`$client show votes`
[ "`echo $res | jq .proposals`" != '[]' ] \
    || { echo "strange proposals" ; exit 1 ; }

bake # pos=2

echo 'Breaking the tie'

$client submit proposals for bootstrap4 ${proto[1]} # To make ${proto[1]} win
$client show votes

bake # pos=3
bake # new period! pos=0

echo The phase must be testing_vote...
res=`$client show votes`
echo $res
[ `echo $res | jq .voting_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '4' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
[ `echo $res | jq .current_period_kind` = '"testing_vote"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`echo $res | jq .listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `echo $res | jq .current_proposal` = '"'${proto[1]}'"' ] \
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

$client show votes
 
bake # new period pos=0
 
echo Testing vote should be done
res=`$client show votes`
[ `echo $res | jq .voting_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '4' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
[ `echo $res | jq .current_period_kind` = '"testing"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`echo $res | jq .listings`" = '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `echo $res | jq .current_proposal` = '"'${proto[1]}'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `echo $res | jq .ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }

bake # pos=1
bake # pos=2
bake # pos=3
bake # new period pos=0
 
echo Testing should be done
res=`$client show votes`
[ `echo $res | jq .voting_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '4' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
[ `echo $res | jq .current_period_kind` = '"promotion_vote"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`echo $res | jq .listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `echo $res | jq .current_proposal` = '"'${proto[1]}'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `echo $res | jq .ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }
 
$client submit ballot for bootstrap1 ${proto[1]} yay
$client submit ballot for bootstrap2 ${proto[1]} yay
$client submit ballot for bootstrap3 ${proto[1]} yay
$client submit ballot for bootstrap4 ${proto[1]} nay # not to promote

bake # pos=1
bake # pos=2
bake # pos=3

$client show votes

bake # new period pos=0
 
echo 'Promotion vote should be over now negatively'
res=`$client show votes`
[ `echo $res | jq .voting_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '4' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
[ `echo $res | jq .current_period_kind` = '"proposal"' ] \
    || { echo "strange current_period_kind" ; exit 1 ; }
[ "`echo $res | jq .listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }
[ `echo $res | jq .current_proposal` = 'null' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `echo $res | jq .ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }
