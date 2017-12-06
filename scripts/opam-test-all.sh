#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

cleanup () {
    set +e
    if [ -f LOG ]; then
        echo "failed."
        echo
        cat LOG
        rm LOG
        echo
        exit 1
    fi
}
trap cleanup EXIT INT

silent () {
    "$@" > LOG 2>&1
    rm LOG
}

requested_packages="$@"

export OPAMYES=yes

echo -n "Cleanup state and pin packages..."
silent ./scripts/opam-unpin.sh
silent . ./scripts/opam-pin.sh
echo " OK."

if ! [ -z "$requested_packages" ]; then
    packages="$requested_packages"
fi

okfile="$0.DONE"
touch $okfile
ok=$(cat "$okfile")

ignore() {
    for i in $ok; do
        if [ $i = $1 ]; then return 0; fi
    done
    return 1
}

for package in $packages; do

    if ignore $package; then
        echo "Ignoring: $package."
        continue
    fi

    echo -n "Installing: $package..."
    silent opam install $package
    echo " OK."

    echo -n "Removing: $package..."
    silent opam remove -a $package
    echo " OK."

    echo $package >> "$okfile"

done

echo
echo "Successfully installed the following packages: "
echo
cat $okfile | sed 's/^/- /'
rm $okfile
