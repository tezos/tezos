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
    for contract in `ls $contract_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi


# Concatenate all strings of a list into one string
#assert_storage $contract_dir/concat_list.tz '""' '{ "a" ; "b" ; "c" }' '"abc"'
#assert_storage $contract_dir/concat_list.tz '""' '{}' '""'
#assert_storage $contract_dir/concat_list.tz \
#			  '""' '{ "Hello" ; " " ; "World" ; "!" }' '"Hello World!"'


# Typing gas bounds checks
assert_fails $client originate contract first_explosion for bootstrap1 \
             transferring 0 from bootstrap1 \
             running '{parameter unit;storage unit;code{DROP;PUSH nat 0;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;}}' -G 8000

# Serialization gas bounds checks
assert_success $client run script  '{parameter (list int);storage (list (list (list int)));code{CAR;DIP{NIL (list int)};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;DIP{NIL (list (list int))};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;NIL operation;PAIR}}' \
               on storage '{}' \
               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'
assert_fails $client run script  '{parameter (list int);storage (list (list (list int)));code{CAR;DIP{NIL (list int)};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;DIP{NIL (list (list int))};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;NIL operation;PAIR}}' \
               on storage '{}' \
               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'


# Test replay prevention
init_with_transfer $contract_dir/replay.tz $key2 Unit 0 bootstrap1
assert_fails $client transfer 0 from bootstrap1 to replay

# Tests create_account
#init_with_transfer $contract_dir/create_account.tz $key2 None 1,000 bootstrap1
#assert_balance create_account "1000 ꜩ"
#created_account=\
#`$client transfer 100 from bootstrap1 to create_account -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' \
#| grep 'New contract' \
#| sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
#| head -1`
#bake
#assert_balance $created_account "100 ꜩ"
#assert_balance create_account "1000 ꜩ"

# Creates a contract, transfers data to it and stores the data
#init_with_transfer $contract_dir/create_contract.tz $key2 Unit 1,000 bootstrap1
#assert_balance create_contract "1000 ꜩ"
#created_contract=\
#`$client transfer 0 from bootstrap1 to create_contract -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' \
#| grep 'New contract' \
#| sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
#| head -1`
#bake
#assert_storage_contains $created_contract '"abcdefg"'
#assert_balance $created_contract "100 ꜩ"
#assert_balance create_contract "900 ꜩ"

# Test IMPLICIT_ACCOUNT
#init_with_transfer $contract_dir/default_account.tz $key1 \
#				   Unit 1,000 bootstrap1
#bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$BOOTSTRAP4_IDENTITY\""
#assert_balance $BOOTSTRAP4_IDENTITY "4000100 ꜩ"
#account=tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5
#bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$account\""
#assert_balance $account "100 ꜩ"

# Test bytes, SHA256, CHECK_SIGNATURE
init_with_transfer $contract_dir/reveal_signed_preimage.tz bootstrap1 \
				   '(Pair 0x9995c2ef7bcc7ae3bd15bdd9b02dc6e877c27b26732340d641a4cbc6524813bb "p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz")' 1,000 bootstrap1
assert_fails $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
             '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972 "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8")'
assert_fails $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
             '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8")'
assert_success $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
               '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f "p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1FhfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ")'
bake


# Test SET_DELEGATE
#b2='tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN'
#b3='tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU'
#b4='tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv'
#b5='tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv'
#init_with_transfer $contract_dir/vote_for_delegate.tz bootstrap1 \
#				   "(Pair (Pair \"$b3\" None) (Pair \"$b4\" None))" 1,000 bootstrap1
#$client get delegate for vote_for_delegate | assert_in_output none

#assert_fails $client transfer 0 from bootstrap1 to vote_for_delegate -arg None
#assert_fails $client transfer 0 from bootstrap2 to vote_for_delegate -arg None
#bake_after $client transfer 0 from bootstrap3 to vote_for_delegate -arg "(Some \"$b5\")"
#assert_storage_contains vote_for_delegate "\"$b5\""
#$client get delegate for vote_for_delegate | assert_in_output none
#bake_after $client transfer 0 from bootstrap4 to vote_for_delegate -arg "(Some \"$b2\")"
#assert_storage_contains vote_for_delegate "\"$b2\""
#$client get delegate for vote_for_delegate | assert_in_output none
#bake_after $client transfer 0 from bootstrap4 to vote_for_delegate -arg "(Some \"$b5\")"
#$client get delegate for vote_for_delegate | assert_in_output "$b5"

# Test sets and map literals
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 0 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ "A" ; "C" ; "B" }' against type '(set string)'
assert_fails $client typecheck data '{ "A" ; "B" ; "B" }' against type '(set string)'

# Test hash consistency between Michelson and the CLI
#hash_result=`$client hash data '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' \
#                     of type '(pair mutez (pair timestamp int))' | grep Blake2b | sed 's/.*: *//'`

#assert_storage $contract_dir/hash_consistency_checker.tz '0x00' \
#              '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' "$hash_result"

#assert_storage $contract_dir/hash_consistency_checker.tz '0x00' \
#              '(Pair 22220000000 (Pair "2017-12-13T04:49:00+00:00" 34))' "$hash_result"

# Test goldenbook

#init_with_transfer $contract_dir/guestbook.tz $key1\
#                   '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" None }' \
#                   100 bootstrap1
#assert_fails $client transfer 0 from bootstrap2 to guestbook -arg '"Pas moi"'
#bake_after $client transfer 0 from bootstrap1 to guestbook -arg '"Coucou"'
#assert_storage_contains guestbook '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" (Some "Coucou") }'
#assert_fails $client transfer 0 from bootstrap3 to guestbook -arg '"Pas moi non plus"'
#assert_fails $client transfer 0 from bootstrap1 to guestbook -arg '"Recoucou ?"'


# Test for issue #262
tee /tmp/bug_262.tz <<EOF
{ parameter unit ; storage unit ; code { DROP ; LAMBDA  unit unit {} ; UNIT ; EXEC ; NIL operation ; PAIR } }
EOF
init_with_transfer /tmp/bug_262.tz $key1 'Unit' 1 bootstrap1
assert_balance bug_262 "1 ꜩ"

printf "\nEnd of test\n"

show_logs="no"
