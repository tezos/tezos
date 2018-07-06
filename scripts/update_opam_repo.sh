#! /bin/sh

set -e

target="$(pwd)"/opam_repo.patch tmp_dir=$(mktemp -dt tezos_deps_opam.XXXXXXXX)

cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$tmp_dir"
    rm -rf Dockerfile
}
# trap cleanup EXIT INT

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

opams=$(find "$src_dir/vendors" "$src_dir/src" -name \*.opam -print)

## Full snapshot of the opam repository
git clone "$opam_repository_url" -b full "$tmp_dir"

## Adding the various tezos packages
packages=
for opam in $opams; do

    dir=$(dirname $opam)
    file=$(basename $opam)
    package=${file%.opam}
    packages=$packages,$package
    mkdir -p "$tmp_dir"/packages/$package/$package.dev

    ## HACK: For some reason, `opam admin list/filter` do not follow
    ## `--with-test/doc` for 'toplevel' package, only for their
    ## 'dependencies.  We want the exact opposite (like for `opam
    ## install`), so we manually remove the tag the most
    ## ugliest-possible way...

    sed -e "s/{ *test *}//" \
        -e "s/test \& //" \
        -e "s/\& test//" \
        -e "s/{ *doc *}//" \
        -e "s/doc \& //" \
        -e "s/\& doc//" \
        $opam > "$tmp_dir"/packages/$package/$package.dev/opam

done

## Filtering unrequired packages
cd $tmp_dir
opam admin filter --yes \
     --resolve $packages,ocaml,ocaml-base-compiler,odoc,opam-depext

## Adding useful compiler variants
for variant in afl flambda fp fp+flambda ; do
    git checkout packages/ocaml-variants/ocaml-variants.$ocaml_version+$variant
done

## Removing the various tezos packages
for opam in $opams; do
    file=$(basename $opam)
    package=${file%.opam}
    rm -r "$tmp_dir"/packages/$package
done

## Adding safer hashes
opam admin add-hashes sha256 sha512

## Generating the diff!
git reset "$opam_repository_tag"
git add packages
git diff HEAD -- packages > "$target"

echo
echo "Wrote proposed update in: $target."
echo 'Please add this patch to: `https://gitlab.com/tezos/opam-repository`'
echo 'And update accordingly the commit hash in: `.gitlab-ci.yml` and `scripts/version.sh`'
echo
