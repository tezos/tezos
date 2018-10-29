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
    for contract in `ls $contract_op_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# delta timestamp
assert_storage $contract_op_dir/add_delta_timestamp.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_op_dir/add_delta_timestamp.tz None '(Pair -100 100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_op_dir/add_delta_timestamp.tz None '(Pair 0 "1970-01-01T00:00:00Z")' '(Some "1970-01-01T00:00:00Z")'

# Test timestamp operations
assert_storage $contract_op_dir/add_timestamp_delta.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_op_dir/add_timestamp_delta.tz None '(Pair 100 -100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_op_dir/add_timestamp_delta.tz None '(Pair "1970-01-01T00:00:00Z" 0)' '(Some "1970-01-01T00:00:00Z")'

# Logical and
assert_storage $contract_op_dir/and.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_op_dir/and.tz None "(Pair False True)" '(Some False)'
assert_storage $contract_op_dir/and.tz None "(Pair True False)" '(Some False)'
assert_storage $contract_op_dir/and.tz None "(Pair True True)" '(Some True)'

# Get the current balance of the contract
assert_storage $contract_op_dir/balance.tz '111' Unit '4000000000000'

# Did the given key sign the string? (key is bootstrap1)

assert_success $client run script $contract_op_dir/check_signature.tz \
               on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "hello")' \
               and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'

assert_fails $client run script $contract_op_dir/check_signature.tz \
             on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "abcd")' \
             and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'


# Concatenate the string in storage with all strings in the given list
assert_storage $contract_op_dir/concat_hello.tz '{}' \
     			  '{ "World!" }' '{ "Hello World!" }'
assert_storage $contract_op_dir/concat_hello.tz '{}' \
     			  '{}' '{}'
assert_storage $contract_op_dir/concat_hello.tz '{}' \
     			  '{ "test1" ; "test2" }' '{ "Hello test1" ; "Hello test2" }'

# Concatenate all strings of a list into one string
assert_storage $contract_op_dir/concat_list.tz '""' '{ "a" ; "b" ; "c" }' '"abc"'
assert_storage $contract_op_dir/concat_list.tz '""' '{}' '""'
assert_storage $contract_op_dir/concat_list.tz \
     			  '""' '{ "Hello" ; " " ; "World" ; "!" }' '"Hello World!"'

# Contains all elements -- does the second list contain all of the same elements
# as the first one? I'm ignoring element multiplicity
assert_storage $contract_op_dir/contains_all.tz \
   			  None '(Pair {} {})' '(Some True)'
assert_storage $contract_op_dir/contains_all.tz \
   			  None '(Pair { "a" } { "B" })' '(Some False)'
assert_storage $contract_op_dir/contains_all.tz \
     		  None '(Pair { "A" } { "B" })' '(Some False)'
assert_storage $contract_op_dir/contains_all.tz \
  			  None '(Pair { "B" } { "B" })' '(Some True)'
assert_storage $contract_op_dir/contains_all.tz None \
 			  '(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })' '(Some True)'
assert_storage $contract_op_dir/contains_all.tz None \
 			  '(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })' '(Some True)'

# different timestamp
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair 0 0)' 0
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair 0 1)' -1
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair 1 0)' 1
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")' 200

# Create an empty map and add a string to it
assert_storage $contract_op_dir/empty_map.tz '{}' Unit \
			  '{ Elt "hello" "world" }'

# Exec concat contract
assert_storage $contract_op_dir/exec_concat.tz '"?"' '""' '"_abc"'
assert_storage $contract_op_dir/exec_concat.tz '"?"' '"test"' '"test_abc"'

# Test get first element of list
assert_storage $contract_op_dir/first.tz '111' '{ 1 ; 2 ; 3 ; 4 }' '1'
assert_storage $contract_op_dir/first.tz '111' '{ 4 }' '4'

# Get the value stored at the given key in the map
assert_storage $contract_op_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '"hello"' '(Pair (Some "hi") { Elt "hello" "hi" })'
assert_storage $contract_op_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '""' '(Pair None { Elt "hello" "hi" })'
assert_storage $contract_op_dir/get_map_value.tz \
			  '(Pair None { Elt "1" "one" ; Elt "2" "two" })' \
			  '"1"' '(Pair (Some "one") { Elt "1" "one" ; Elt "2" "two" })'

# Test hash consistency between Michelson and the CLI
hash_result=`$client hash data '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' \
           of type '(pair mutez (pair timestamp int))' | grep Blake2b | sed 's/.*: *//'`

assert_storage $contract_op_dir/hash_consistency_checker.tz '0x00' \
          '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' "$hash_result"

assert_storage $contract_op_dir/hash_consistency_checker.tz '0x00' \
          '(Pair 22220000000 (Pair "2017-12-13T04:49:00+00:00" 34))' "$hash_result"

# Convert a public key to a public key hash
assert_storage $contract_op_dir/hash_key.tz None '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' \
          '(Some "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")'
assert_storage $contract_op_dir/hash_key.tz None '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"' \
          '(Some "tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k")'

bake_after $client transfer 1,000 from bootstrap1 to $key1
bake_after $client transfer 2,000 from bootstrap1 to $key2

assert_balance $key1 "1000 ꜩ"
assert_balance $key2 "2000 ꜩ"

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_storage $contract_op_dir/hash_string.tz '0x00' '"abcdefg"' '0x46fdbcb4ea4eadad5615cdaa17d67f783e01e21149ce2b27de497600b4cd8f4e'
assert_storage $contract_op_dir/hash_string.tz '0x00' '"12345"' '0xb4c26c20de52a4eaf0d8a340db47ad8cb1e74049570859c9a9a3952b204c772f'

# IF_SOME
assert_storage $contract_op_dir/if_some.tz '"?"' '(Some "hello")' '"hello"'
assert_storage $contract_op_dir/if_some.tz '"?"' 'None' '""'

# Return True if True branch of if was taken and False otherwise
assert_storage $contract_op_dir/if.tz None True '(Some True)'
assert_storage $contract_op_dir/if.tz None False '(Some False)'

# Test addition and subtraction on tez
assert_storage $contract_op_dir/tez_add_sub.tz None '(Pair 2000000 1000000)' '(Some (Pair 3000000 1000000))'
assert_storage $contract_op_dir/tez_add_sub.tz None '(Pair 2310000 1010000)' '(Some (Pair 3320000 1300000))'
