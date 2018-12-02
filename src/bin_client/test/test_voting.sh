#!/bin/bash

# Requires jq command

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

# Prepare a config with shorter blocks_per_voting_period
temp=`mktemp`
sed -e 's/"blocks_per_voting_period" : [0-9]*/"blocks_per_voting_period" : 4/' $parameters_file > $temp
parameters_file=$temp
echo params=${parameters_file}

start_node 1
activate_alpha

echo Alpha activated

res=`$client show votes`
echo $res

[ `echo $res | jq .voting_period_position` = '1' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '3' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
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

res=`$client show votes`
[ `echo $res | jq .voting_period_position` = '0' ] \
    || { echo "strange voting_period_position" ; exit 1 ; }
[ `echo $res | jq .voting_period_remaining` = '4' ] \
    || { echo "strange voting_period_remaining" ; exit 1 ; }
[ "`echo $res | jq .listings`" != '[]' ] \
    || { echo "strange listings" ; exit 1 ; }

proto1='ProtoBetaBetaBetaBetaBetaBetaBetaBetaBet11111a5ug96'
proto2='Proto222222222222222222222222222222222225b7e3dV844j'
proto3='Proto33333333333333333333333333333333333c6379eVysnU'

$client submit proposals for bootstrap1 $proto1
$client submit proposals for bootstrap2 $proto1 $proto2
$client submit proposals for bootstrap3 $proto2
$client submit proposals for bootstrap4 $proto3

bake

res=`$client show votes`
[ "`echo $res | jq .proposals`" != '[]' ] \
    || { echo "strange proposals" ; exit 1 ; }

bake # pos=2

echo Breaking the tie

$client submit proposals for bootstrap3 $proto1 # To make $proto1 win
$client show votes

bake # pos=3
bake # new period! pos=0

echo Proposal should be done
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
[ `echo $res | jq .current_proposal` = '"'$proto1'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
 
echo Ballots 1
$client submit ballot for bootstrap1 $proto1 yay
$client submit ballot for bootstrap2 $proto1 yay
$client submit ballot for bootstrap3 $proto1 yay
$client submit ballot for bootstrap4 $proto1 yay
 
bake # pos=1

# They cannot change their mind.
echo "Ballots 2 (should fail)"
$client submit ballot for bootstrap1 $proto1 yay \
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
[ `echo $res | jq .current_proposal` = '"'$proto1'"' ] \
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
[ `echo $res | jq .current_proposal` = '"'$proto1'"' ] \
    || { echo "strange current_proposal" ; exit 1 ; }
[ `echo $res | jq .ballot_list` = '[]' ] \
    || { echo "strange ballot_list" ; exit 1 ; }
 
$client submit ballot for bootstrap1 $proto1 yay
$client submit ballot for bootstrap2 $proto1 yay
$client submit ballot for bootstrap3 $proto1 yay
$client submit ballot for bootstrap4 $proto1 nay # not to promote

bake # pos=1
bake # pos=2
bake # pos=3

$client show votes

bake # new period pos=0
 
echo 'Promotion vote should be done (negatively)'
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
