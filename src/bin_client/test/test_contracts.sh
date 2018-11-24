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
    for contract in `ls $contract_attic_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# Typing gas bounds checks
tee /tmp/first_explosion.tz <<EOF
{ parameter unit;
  storage unit;
  code{ DROP; PUSH nat 0 ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR } }'
EOF
assert_fails $client originate contract first_explosion for bootstrap1 \
             transferring 0 from bootstrap1 \
             running /tmp/first_explosion.tz -G 8000 --burn-cap 10

# Serialization gas bounds checks
tee /tmp/second_explosion.tz <<EOF
{ parameter (list int) ;
  storage (list (list (list int))) ;
  code { CAR ; DIP { NIL (list int) } ;
         DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
         DROP ; DIP { NIL (list (list int)) } ;
         DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
         DROP ; NIL operation ; PAIR } }
EOF
assert_success $client run script /tmp/second_explosion.tz \
               on storage '{}' \
               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'
#assert_fails $client run script /tmp/second_explosion.tz \
#               on storage '{}' \
#               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'

# Test sets and map literals
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 0 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ "A" ; "C" ; "B" }' against type '(set string)'
assert_fails $client typecheck data '{ "A" ; "B" ; "B" }' against type '(set string)'

# Test for issue #262 (bad serialization of 3+ arity primitives)
tee /tmp/bug_262.tz <<EOF
{ parameter unit ;
  storage unit ;
  code { DROP ;
         LAMBDA unit unit {} ; UNIT ; EXEC ;
         NIL operation ; PAIR } }
EOF
init_with_transfer /tmp/bug_262.tz $key1 'Unit' 1 bootstrap1
assert_balance bug_262 "1 ꜩ"

printf "\nEnd of test\n"

show_logs="no"
