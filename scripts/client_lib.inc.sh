#! /bin/bash

client=${client:=tezos-client}

function wait_for_the_node_to_be_ready() {
    local last_level
    local current_level
    while ! $client rpc call blocks/head/proto/context/level >/dev/null 2>&1
    do
        echo Waiting 10 seconds for the node to launch...
        sleep 10
    done
    ## Wait for the level not to change for 30sec...
    last_level="$($client rpc call blocks/head/proto/context/level)"
    while [ "$last_level" != "$current_level" ]
    do
        echo Waiting 30 seconds for the node to synchronize...
        sleep 30
        last_level=$current_level
        current_level="$($client rpc call blocks/head/proto/context/level)"
    done
}

function wait_for_a_new_block() {
    local last_level
    local current_level
    last_level="$($client rpc call blocks/head/proto/context/level)"
    current_level="$last_level"
    while [ "$last_level" = "$current_level" ]
    do
        echo Waiting 10 seconds for a block to be baked...
        sleep 10
        current_level="$($client rpc call blocks/head/proto/context/level)"
    done
}

function create_identity() {
    if ! $client get balance my_identity 2> /dev/null 1> /dev/null
    then
        $client gen keys my_identity
    fi
}

function create_account() {
    while ! $client get balance my_account 2> /dev/null 1> /dev/null
    do
        echo Emiting origination...
        $client forget contract my_account || true
        $client originate account my_account for my_identity \
                transfering 50,000.00 from bootstrap1
        wait_for_a_new_block
    done
    echo Adding some credit in the new account...
    $client transfer 50,000.00 from bootstrap2 to my_account
    $client transfer 50,000.00 from bootstrap3 to my_account
    $client transfer 50,000.00 from bootstrap4 to my_account
    echo Provisining some credit for bond deposit...
    $client transfer 50,000.00 from bootstrap5 to my_identity
    wait_for_a_new_block
}

function wait_for_the_account() {
    local account
    while [ -z "$account" ]
    do
        echo Waiting 10 seconds for baker to create the account...
        sleep 10
        for contract in $($client list contracts \
                              | grep -v "(default)" \
                              | awk '{ print $1; }')
        do
            if $client get manager "$contract" | grep my_identity; then
                account=$($client get manager "$contract" | awk '{ print $1; }')
                echo account created
            fi
        done
    done
    $client remember contract my_account "$account"
}

function start_baker() {
    declare -a identities
    if [ $# -eq 0 ]; then
        identities[0]=my_identity
    else
        identities=("$@")
    fi
    echo "Start baking for ${identities[*]}..."
    $client launch daemon -mining -max-priority 64 "${identities[@]}"
}

function start_endorser() {
    declare -a identities
    if [ $# -eq 0 ]; then
        identities[0]=my_identity
    else
        identities=("$@")
    fi
    echo "Start endorsing for ${identities[*]}..."
    $client launch daemon -endorsement "${identities[@]}"
}

