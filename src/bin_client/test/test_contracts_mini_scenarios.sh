#!/bin/bash

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

$client -w none config update

bake

key1=foo
key2=bar

$client gen keys $key1
$client gen keys $key2

printf "\n\n"

# Assert all contracts typecheck
if [ ! $NO_TYPECHECK ] ; then
    for contract in `ls $contract_scenarios_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# FORMAT: assert_output contract_file storage input expected_result

# TODO add tests for the following contracts
# lockup, originator, parameterized_multisig, reservoir, scrutable_reservoir,
# weather_insurance, xcat_dapp, xcat
# NB: hardlimit.tz is tested in test_basic.sh

# Test replay prevention
init_with_transfer $contract_scenarios_dir/replay.tz $key2 Unit 0 bootstrap1
assert_fails $client transfer 0 from bootstrap1 to replay --burn-cap 10

# Tests create_account
init_with_transfer $contract_scenarios_dir/create_account.tz $key2 None 1,000 bootstrap1
assert_balance create_account "1000 ꜩ"
created_account=\
`$client transfer 100 from bootstrap1 to create_account -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' --burn-cap 10 \
 | grep 'New contract' \
 | sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
 | head -1`
bake
assert_balance $created_account "100 ꜩ"
assert_balance create_account "1000 ꜩ"

# Creates a contract, transfers data to it and stores the data
init_with_transfer $contract_scenarios_dir/create_contract.tz $key2 Unit 1,000 bootstrap1
assert_balance create_contract "1000 ꜩ"
created_contract=\
`$client transfer 0 from bootstrap1 to create_contract -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' --burn-cap 10 \
| grep 'New contract' \
| sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
| head -1`
bake
assert_storage_contains $created_contract '"abcdefg"'
assert_balance $created_contract "100 ꜩ"
assert_balance create_contract "900 ꜩ"

# Test IMPLICIT_ACCOUNT
init_with_transfer $contract_scenarios_dir/default_account.tz $key1 \
				   Unit 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$BOOTSTRAP4_IDENTITY\"" --burn-cap 10
assert_balance $BOOTSTRAP4_IDENTITY "4000100 ꜩ"
account=tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5
bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$account\"" --burn-cap 10
assert_balance $account "100 ꜩ"

# Test bytes, SHA256, CHECK_SIGNATURE
init_with_transfer $contract_scenarios_dir/reveal_signed_preimage.tz bootstrap1 \
				   '(Pair 0x9995c2ef7bcc7ae3bd15bdd9b02dc6e877c27b26732340d641a4cbc6524813bb "p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz")' 1,000 bootstrap1
assert_fails $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
             '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972 "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8")'  --burn-cap 10
assert_fails $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
             '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8")'  --burn-cap 10
assert_success $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
               '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f "p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1FhfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ")'  --burn-cap 10
bake

# Test SET_DELEGATE
b2='tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN'
b3='tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU'
b4='tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv'
b5='tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv'
init_with_transfer $contract_scenarios_dir/vote_for_delegate.tz bootstrap1 \
				   "(Pair (Pair \"$b3\" None) (Pair \"$b4\" None))" 1,000 bootstrap1
$client get delegate for vote_for_delegate | assert_in_output none

assert_fails $client transfer 0 from bootstrap1 to vote_for_delegate -arg None --burn-cap 10
assert_fails $client transfer 0 from bootstrap2 to vote_for_delegate -arg None --burn-cap 10
bake_after $client transfer 0 from bootstrap3 to vote_for_delegate -arg "(Some \"$b5\")" --burn-cap 10
assert_storage_contains vote_for_delegate "\"$b5\""
$client get delegate for vote_for_delegate | assert_in_output none
bake_after $client transfer 0 from bootstrap4 to vote_for_delegate -arg "(Some \"$b2\")" --burn-cap 10
assert_storage_contains vote_for_delegate "\"$b2\""
$client get delegate for vote_for_delegate | assert_in_output none
bake_after $client transfer 0 from bootstrap4 to vote_for_delegate -arg "(Some \"$b5\")" --burn-cap 10
$client get delegate for vote_for_delegate | assert_in_output "$b5"


printf "\nEnd of test\n"

show_logs="no"
