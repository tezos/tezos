#! /bin/sh

client_dir="${client_dir:=$HOME/.tezos-client}"
client="${client:=tezos-client -base-dir $client_dir}"

## Waiter ##################################################################

wait_for_the_node_to_be_ready() {
    local count=0
    if $client rpc call blocks/head/hash >/dev/null 2>&1; then return; fi
    printf "Waiting for the node to initialize..."
    sleep 1
    while ! $client rpc call blocks/head/hash >/dev/null 2>&1
    do
        count=$((count+1))
        if [ "$count" -ge 30 ]; then
            echo " timeout."
            exit 2
        fi
        printf "."
        sleep 1
    done
    echo " done."
}

wait_for_the_node_to_be_bootstraped() {
    wait_for_the_node_to_be_ready
    echo "Waiting for the node to synchronize with the network..."
    $client bootstrapped
}

## Account #################################################################

may_create_identity() {
    if ! $client get balance for "my_identity" >/dev/null 2>&1 ; then
        echo "Generating new manager key (known as 'my_identity')..."
        $client gen keys "my_identity"
    fi
    if ! $client get balance for "my_account" >/dev/null 2>&1 ; then
        echo "Creating new account for 'my_identity' (known as 'my_account')..."
        $client forget contract "my_account" >/dev/null 2>&1 || true
        $client originate free account "my_account" for "my_identity"
    fi
}

## Baker ###################################################################

check_baker() {
    pids=$(pgrep -x tezos-client 2>/dev/null)
    for pid in $pids; do
        if grep -- "-mining" "/proc/$pid/cmdline" >/dev/null 2>&1 ; then
            return 0
        fi
    done
    return 1
}

run_baker() {
    if check_baker; then
        echo "Cannot run two bakers in the same container."
        exit 1
    fi
    echo "Start baker..."
    exec $client launch daemon -mining -max-priority 64 "$@" > "$client_dir/baker.log"
}

stop_baker() {
    pids=$(pgrep -x tezos-client 2>/dev/null)
    for pid in $pids; do
        if grep -- "-mining" "/proc/$pid/cmdline" >/dev/null 2>&1 ; then
            echo "Killing the baker..."
            kill "$pid"
        fi
    done
}

log_baker() {
    if ! check_baker ; then
        echo
        echo "##############################################"
        echo "##                                          ##"
        echo "## Warning: The tezos baker is not running! ##"
        echo "##                                          ##"
        echo "##############################################"
        echo
        tail "$client_dir/baker.log"
    else
        tail -f "$client_dir/baker.log"
    fi
}

## Endorser ################################################################

check_endorser() {
    pids=$(pgrep -x tezos-client 2>/dev/null)
    for pid in $pids; do
        if grep -- "-endorsement" "/proc/$pid/cmdline" > /dev/null 2>&1 ; then
            return 0
        fi
    done
    return 1
}

run_endorser() {
    if check_endorser; then
        echo "Cannot run two endorsers in the same container."
        exit 1
    fi
    echo "Start endorser..."
    exec $client launch daemon -endorsement "$@" > "$client_dir/endorser.log"
}

stop_endorser() {
    pids=$(pgrep -x tezos-client 2>/dev/null)
    for pid in $pids; do
        if grep -- "-endorsement" "/proc/$pid/cmdline" > /dev/null 2>&1 ; then
            kill "$pid"
        fi
    done
}

log_endorser() {
    if ! check_endorser ; then
        echo
        echo "#################################################"
        echo "##                                             ##"
        echo "## Warning: The tezos endorser is not running! ##"
        echo "##                                             ##"
        echo "#################################################"
        echo
        tail "$client_dir/endorser.log"
    else
        tail -f "$client_dir/endorser.log"
    fi
}

## Sandboxed client ########################################################

# key pairs from $src_dir/test/sandbox.json

BOOTSTRAP1_PUBLIC="edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
BOOTSTRAP1_SECRET="edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi"

BOOTSTRAP2_PUBLIC="edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9"
BOOTSTRAP2_SECRET="edskRkJz4Rw2rM5NtabEWMbbg2bF4b1nfFajaqEuEk4SgU7eeDbym9gVQtBTbYo32WUg2zb5sNBkD1whRN7zX43V9bftBbtaKc"

BOOTSTRAP3_PUBLIC="edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV"
BOOTSTRAP3_SECRET="edskS3qsqsNgdjUqeMsVcEwBn8dkZ5iDRz6aF21KhcCtRiAkWBypUSbicccR4Vgqm9UdW2Vabuos6seezqgbXTrmcbLUG4rdAC"

BOOTSTRAP4_PUBLIC="edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU"
BOOTSTRAP4_SECRET="edskRg9qcPqaVQa6jXWNMU5p71tseSuR7NzozgqZ9URsVDi81wTyPJdFSBdeakobyHUi4Xgu61jgKRQvkhXrPmEdEUfiqfiJFL"

BOOTSTRAP5_PUBLIC="edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n"
BOOTSTRAP5_SECRET="edskS7rLN2Df3nbS1EYvwJbWo4umD7yPM1SUeX7gp1WhCVpMFXjcCyM58xs6xsnTsVqHQmJQ2RxoAjJGedWfvFmjQy6etA3dgZ"

DICTATOR_SECRET="edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z"

add_sandboxed_bootstrap_identities() {

    ${client} add public key bootstrap1 ${BOOTSTRAP1_PUBLIC}
    ${client} add secret key bootstrap1 ${BOOTSTRAP1_SECRET}

    ${client} add public key bootstrap2 ${BOOTSTRAP2_PUBLIC}
    ${client} add secret key bootstrap2 ${BOOTSTRAP2_SECRET}

    ${client} add public key bootstrap3 ${BOOTSTRAP3_PUBLIC}
    ${client} add secret key bootstrap3 ${BOOTSTRAP3_SECRET}

    ${client} add public key bootstrap4 ${BOOTSTRAP4_PUBLIC}
    ${client} add secret key bootstrap4 ${BOOTSTRAP4_SECRET}

    ${client} add public key bootstrap5 ${BOOTSTRAP5_PUBLIC}
    ${client} add secret key bootstrap5 ${BOOTSTRAP5_SECRET}

    ${client} add secret key dictator ${DICTATOR_SECRET}

}
