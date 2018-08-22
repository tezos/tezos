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

# Test ASSERT
assert_storage $contract_op_dir/assert.tz Unit True Unit
assert_fails $client run script $contract_op_dir/assert.tz on storage Unit and input False

# COMPARE; ASSERT_

assert_storage $contract_op_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_op_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_op_dir/assert_neq.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_neq.tz on storage Unit and input '(Pair -1 -1)'

assert_storage $contract_op_dir/assert_lt.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_op_dir/assert_lt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails $client run script $contract_op_dir/assert_lt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_op_dir/assert_le.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_op_dir/assert_le.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_op_dir/assert_le.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_op_dir/assert_gt.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_gt.tz on storage Unit and input '(Pair -1 0)'
assert_fails $client run script $contract_op_dir/assert_gt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_op_dir/assert_ge.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_op_dir/assert_ge.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_ge.tz on storage Unit and input '(Pair -1 0)'

# ASSERT_CMP
assert_storage $contract_op_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_op_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_op_dir/assert_cmpneq.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_cmpneq.tz on storage Unit and input '(Pair -1 -1)'

assert_storage $contract_op_dir/assert_cmplt.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_op_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails $client run script $contract_op_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_op_dir/assert_cmple.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_op_dir/assert_cmple.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_op_dir/assert_cmple.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_op_dir/assert_cmpgt.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_cmpgt.tz on storage Unit and input '(Pair -1 0)'
assert_fails $client run script $contract_op_dir/assert_cmpgt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_op_dir/assert_cmpge.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_op_dir/assert_cmpge.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_op_dir/assert_cmpge.tz on storage Unit and input '(Pair -1 0)'
