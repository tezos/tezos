
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
    for contract in `ls $contract_macros_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

assert_storage $contract_macros_dir/add_delta_timestamp.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_macros_dir/add_delta_timestamp.tz None '(Pair -100 100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_macros_dir/add_delta_timestamp.tz None '(Pair 0 "1970-01-01T00:00:00Z")' '(Some "1970-01-01T00:00:00Z")'

# Logical and
assert_storage $contract_macros_dir/and.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_macros_dir/and.tz None "(Pair False True)" '(Some False)'
assert_storage $contract_macros_dir/and.tz None "(Pair True False)" '(Some False)'
assert_storage $contract_macros_dir/and.tz None "(Pair True True)" '(Some True)'

# Get the current balance of the contract
assert_storage $contract_macros_dir/balance.tz '111' Unit '4000000000000'


# Test for big maps
init_with_transfer $contract_macros_dir/big_map_mem.tz $key1\
                   '(Pair { Elt 1 Unit ; Elt 2 Unit ; Elt 3 Unit } Unit)' \
                   100 bootstrap1
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 0 False)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 0 True)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 1 True)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 1 False)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 2 True)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 2 False)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 3 True)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 3 False)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 4 False)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 4 True)'
assert_fails $client typecheck data '3' against type \
             '(int @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)'
$client typecheck data '3' against type \
        '(int @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)'

init_with_transfer $contract_macros_dir/big_map_get_add.tz $key1\
                   '(Pair { Elt 0 1 ; Elt 1 2 ; Elt 2 3 } Unit)' \
                   100 bootstrap1

bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 (Some 2)) (Pair 200 (Some 2)))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 None) (Pair 200 None))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 None) (Pair 300 None))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 1 None) (Pair 200 None))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 1 (Some 2)) (Pair 0 (Some 1)))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 400 (Some 1232)) (Pair 400 (Some 1232)))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 401 (Some 0)) (Pair 400 (Some 1232)))'


# Build list
assert_storage $contract_macros_dir/build_list.tz '{}' 0 "{ 0 }"
assert_storage $contract_macros_dir/build_list.tz '{}' 3 "{ 0 ; 1 ; 2 ; 3 }"
assert_storage $contract_macros_dir/build_list.tz '{}' 10 \
			  "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }"

# Did the given key sign the string? (key is bootstrap1)

assert_success $client run script $contract_macros_dir/check_signature.tz \
               on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "hello")' \
               and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'

assert_fails $client run script $contract_macros_dir/check_signature.tz \
             on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "abcd")' \
             and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'

# Test comparisons on bytes { EQ ; GT ; LT ; GE ; LE }
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x33 0x34)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x33 0x33aa)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x33 0x33)' '{ True ; False ; False ; True ; True }'
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x34 0x33)' '{ False ; True ; False ; True ; False }'

# Test comparisons on tez { EQ ; GT ; LT ; GE ; LE }
assert_storage $contract_macros_dir/compare.tz '{}' '(Pair 1000000 2000000)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_macros_dir/compare.tz '{}' '(Pair 2000000 1000000)' '{ False ; True ; False ; True ; False }'
assert_storage $contract_macros_dir/compare.tz '{}' '(Pair 2370000 2370000)' '{ True ; False ; False ; True ; True }'

# Concatenate the string in storage with all strings in the given list
assert_storage $contract_macros_dir/concat_hello.tz '{}' \
			  '{ "World!" }' '{ "Hello World!" }'
assert_storage $contract_macros_dir/concat_hello.tz '{}' \
			  '{}' '{}'
assert_storage $contract_macros_dir/concat_hello.tz '{}' \
			  '{ "test1" ; "test2" }' '{ "Hello test1" ; "Hello test2" }'

# Concatenate all strings of a list into one string
assert_storage $contract_macros_dir/concat_list.tz '""' '{ "a" ; "b" ; "c" }' '"abc"'
assert_storage $contract_macros_dir/concat_list.tz '""' '{}' '""'
assert_storage $contract_macros_dir/concat_list.tz \
			  '""' '{ "Hello" ; " " ; "World" ; "!" }' '"Hello World!"'

# Contains all elements -- does the second list contain all of the same elements
# as the first one? I'm ignoring element multiplicity
assert_storage $contract_macros_dir/contains_all.tz \
			  None '(Pair {} {})' '(Some True)'
assert_storage $contract_macros_dir/contains_all.tz \
			  None '(Pair { "a" } { "B" })' '(Some False)'
assert_storage $contract_macros_dir/contains_all.tz \
  		  None '(Pair { "A" } { "B" })' '(Some False)'
assert_storage $contract_macros_dir/contains_all.tz \
			  None '(Pair { "B" } { "B" })' '(Some True)'
assert_storage $contract_macros_dir/contains_all.tz None \
			  '(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })' '(Some True)'
assert_storage $contract_macros_dir/contains_all.tz None \
			  '(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })' '(Some True)'

# Tests create_account
init_with_transfer $contract_macros_dir/create_account.tz $key2 None 1,000 bootstrap1
assert_balance create_account "1000 ꜩ"
created_account=\
`$client transfer 100 from bootstrap1 to create_account -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' \
 | grep 'New contract' \
 | sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
 | head -1`
bake
assert_balance $created_account "100 ꜩ"
assert_balance create_account "1000 ꜩ"

assert_storage $contract_macros_dir/diff_timestamps.tz 111 '(Pair 0 0)' 0
assert_storage $contract_macros_dir/diff_timestamps.tz 111 '(Pair 0 1)' -1
assert_storage $contract_macros_dir/diff_timestamps.tz 111 '(Pair 1 0)' 1
assert_storage $contract_macros_dir/diff_timestamps.tz 111 '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")' 200

# Create an empty map and add a string to it
assert_storage $contract_macros_dir/empty_map.tz '{}' Unit \
			  '{ Elt "hello" "world" }'

# Exec concat contract
assert_storage $contract_macros_dir/exec_concat.tz '"?"' '""' '"_abc"'
assert_storage $contract_macros_dir/exec_concat.tz '"?"' '"test"' '"test_abc"'

# Test get first element of list
assert_storage $contract_macros_dir/first.tz '111' '{ 1 ; 2 ; 3 ; 4 }' '1'
assert_storage $contract_macros_dir/first.tz '111' '{ 4 }' '4'

# Get the value stored at the given key in the map
assert_storage $contract_macros_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '"hello"' '(Pair (Some "hi") { Elt "hello" "hi" })'
assert_storage $contract_macros_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '""' '(Pair None { Elt "hello" "hi" })'
assert_storage $contract_macros_dir/get_map_value.tz \
			  '(Pair None { Elt "1" "one" ; Elt "2" "two" })' \
			  '"1"' '(Pair (Some "one") { Elt "1" "one" ; Elt "2" "two" })'

# Test goldenbook
init_with_transfer $contract_macros_dir/guestbook.tz $key1\
       '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" None }' \
       100 bootstrap1
assert_fails $client transfer 0 from bootstrap2 to guestbook -arg '"Pas moi"'
bake_after $client transfer 0 from bootstrap1 to guestbook -arg '"Coucou"'
assert_storage_contains guestbook '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" (Some "Coucou") }'
assert_fails $client transfer 0 from bootstrap3 to guestbook -arg '"Pas moi non plus"'
assert_fails $client transfer 0 from bootstrap1 to guestbook -arg '"Recoucou ?"'

# Test hash consistency between Michelson and the CLI
hash_result=`$client hash data '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' \
                     of type '(pair mutez (pair timestamp int))' | grep Blake2b | sed 's/.*: *//'`

assert_storage $contract_macros_dir/hash_consistency_checker.tz '0x00' \
              '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' "$hash_result"

assert_storage $contract_macros_dir/hash_consistency_checker.tz '0x00' \
              '(Pair 22220000000 (Pair "2017-12-13T04:49:00+00:00" 34))' "$hash_result"

# Convert a public key to a public key hash
assert_storage $contract_macros_dir/hash_key.tz None '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' \
              '(Some "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")'
assert_storage $contract_macros_dir/hash_key.tz None '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"' \
              '(Some "tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k")'

bake_after $client transfer 1,000 from bootstrap1 to $key1
bake_after $client transfer 2,000 from bootstrap1 to $key2

assert_balance $key1 "1000 ꜩ"
assert_balance $key2 "2000 ꜩ"

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_storage $contract_macros_dir/hash_string.tz '0x00' '"abcdefg"' '0x46fdbcb4ea4eadad5615cdaa17d67f783e01e21149ce2b27de497600b4cd8f4e'
assert_storage $contract_macros_dir/hash_string.tz '0x00' '"12345"' '0xb4c26c20de52a4eaf0d8a340db47ad8cb1e74049570859c9a9a3952b204c772f'

# IF_SOME
assert_storage $contract_macros_dir/if_some.tz '"?"' '(Some "hello")' '"hello"'
assert_storage $contract_macros_dir/if_some.tz '"?"' 'None' '""'

# Return True if True branch of if was taken and False otherwise
assert_storage $contract_macros_dir/if.tz None True '(Some True)'
assert_storage $contract_macros_dir/if.tz None False '(Some False)'

# Test addition and subtraction on tez
assert_storage $contract_macros_dir/tez_add_sub.tz None '(Pair 2000000 1000000)' '(Some (Pair 3000000 1000000))'
assert_storage $contract_macros_dir/tez_add_sub.tz None '(Pair 2310000 1010000)' '(Some (Pair 3320000 1300000))'
