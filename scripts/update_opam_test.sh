#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir/opam-pin.sh"

tmp=$(mktemp)

sed -z 's/^\(.*##BEGIN_OPAM##\n\).*\(\n##END_OPAM##.*\)$/\1/' "$src_dir/.gitlab-ci.yml" > $tmp

cpt=0
for package in $packages; do
    num=$(printf "%02d" $cpt)
    cpt=$((cpt+1))
    cat >> $tmp <<EOF
opam:$num:$package:
  <<: *opam_definition
  variables:
    package: $package

EOF
done

sed -z 's/^\(.*##BEGIN_OPAM##\n\).*\(\n##END_OPAM##.*\)$/\2/' "$src_dir/.gitlab-ci.yml" >> $tmp

mv $tmp "$src_dir/.gitlab-ci.yml"

