#! /bin/sh

client_dir="${client_dir:=$HOME/.tezos-client}"
client="${client:=tezos-client -base-dir $client_dir}"

## Waiter ##################################################################

wait_for_the_node_to_be_ready() {
    if $client rpc call blocks/head/hash >/dev/null 2>&1; then return; fi
    printf "Waiting for the node to initialize..."
    sleep 1
    while ! $client rpc call blocks/head/hash >/dev/null 2>&1
    do
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
    if ! $client get balance "my_identity" >/dev/null 2>&1 ; then
        echo "Generating new manager key (known as 'my_identity')..."
        $client gen keys "my_identity"
    fi
    if ! $client get balance "my_account" >/dev/null 2>&1 ; then
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

