#! /usr/bin/env bash

set -e

client_dir="${client_dir:=$HOME/.tezos-client}"
client="${client:=tezos-client -base-dir $client_dir}"

client_dirs=()

init_sandboxed_client() {

    id="$1"
    shift 1

    rpc=$((18730 + id))
    client_dir="$(mktemp -d -t tezos-client.XXXXXXXX)"
    client_dirs+=("$client_dir")
    client="$local_client -base-dir $client_dir -addr 127.0.0.1 -port $rpc"

}

cleanup_clients() {
    rm -rf "${client_dirs[@]}"
}


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
        if grep -- "-baking" "/proc/$pid/cmdline" >/dev/null 2>&1 ; then
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
    exec $client launch daemon -baking -max-priority 64 "$@" > "$client_dir/baker.log"
}

stop_baker() {
    pids=$(pgrep -x tezos-client 2>/dev/null)
    for pid in $pids; do
        if grep -- "-baking" "/proc/$pid/cmdline" >/dev/null 2>&1 ; then
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

BOOTSTRAP1_IDENTITY="tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
BOOTSTRAP1_PUBLIC="edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
BOOTSTRAP1_SECRET="edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh"

BOOTSTRAP2_IDENTITY="tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"
BOOTSTRAP2_PUBLIC="edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9"
BOOTSTRAP2_SECRET="edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo"

BOOTSTRAP3_IDENTITY="tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
BOOTSTRAP3_PUBLIC="edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV"
BOOTSTRAP3_SECRET="edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ"

BOOTSTRAP4_IDENTITY="tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv"
BOOTSTRAP4_PUBLIC="edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU"
BOOTSTRAP4_SECRET="edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3"

BOOTSTRAP5_IDENTITY="tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"
BOOTSTRAP5_PUBLIC="edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n"
BOOTSTRAP5_SECRET="edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm"

DICTATOR_SECRET="edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"

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

activate_alpha() {

    ${client} \
        -block genesis \
        activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
        with fitness 1 \
        and passes 1 \
        and key dictator

}

usage() {
    echo "Small script to initialize a client to a local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: eval \`$0 <id>\`"
    echo "  where <id> should be an integer between 1 and 9."
}

main () {

    local bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
    if [ $(basename "$bin_dir") = "bin_client" ]; then
      local_client="${local_client:-$bin_dir/../_build/default/bin_client/main.exe}"
    fi

    if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
        usage
        exit 1
    fi

    init_sandboxed_client "$1"

    add_sandboxed_bootstrap_identities | sed -e 's/^/## /' 1>&2

    cat <<EOF
if type tezos-client-reset >/dev/null 2>&1 ; then tezos-client-reset; fi ;
alias tezos-client="$client" ;
alias tezos-activate-alpha="$client -block genesis activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK with fitness 1 and passes 1 and key dictator" ;
alias tezos-client-reset="rm -rf \"$client_dir\"; unalias tezos-client tezos-activate-alpha tezos-client-reset" ;
alias tezos-autocomplete="source \"$bin_dir/bash-completion.sh\"" ;
trap tezos-client-reset EXIT ;
EOF

    (cat | sed -e 's/^/## /') 1>&2 <<EOF

The client is now properly initialized. In the rest of this shell
session, you might now run \`tezos-client\` to communicate with a
tezos node launched with \`launch-sandboxed-node $1\`. For instance:

  tezos-client rpc call blocks/head/protocol

Note: if the current protocol version, as reported by the previous
command, is "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im", you
may have to activate in your "sandboxed network" the same economic
protocol than used by the alphanet by running:

  tezos-activate-alpha

Warning: all the client data will be removed when you close this shell
or if you run this command a second time.

Activate tab completion by running:

  tezos-autocomplete

EOF

}

if [ "$0" == "$BASH_SOURCE" ]; then
    main "$@"
fi
