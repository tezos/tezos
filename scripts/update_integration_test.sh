#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

tmp=$(mktemp)

sed -z 's/^\(.*##BEGIN_INTEGRATION_PYTHON##\n\).*\(\n##END_INTEGRATION_PYTHON##.*\)$/\1/' "$src_dir/.gitlab-ci.yml" > $tmp

cpt=0
for test in tests_python/tests/test_*.py; do
    num=$(printf "%02d" $cpt)
    cpt=$((cpt+1))
    testname=${test##tests_python/tests/test_}
    testname=${testname%%.py}
    cat >> $tmp <<EOF
integration:$num:$testname:
  <<: *integration_definition
  script:
    - pytest $test

EOF
done

sed -z 's/^\(.*##BEGIN_INTEGRATION_PYTHON##\n\).*\(\n##END_INTEGRATION_PYTHON##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"

