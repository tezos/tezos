#! /bin/sh

data_dir="/var/run/tezos"
node_dir="${node_dir:=/var/run/tezos/node}"
client_dir="${client_dir:=/var/run/tezos/client}"
node="${node:=tezos-node}"
client="${client:=tezos-client -base-dir \"$client_dir\"}"

init() {
    if [ ! -f "$data_dir/alphanet_version" ] || \
       [ "$(cat "$data_dir/alphanet_version")"  \
         != "$(cat ~/scripts/alphanet_version)" ]; then
        echo -e "\033[33mThe alphanet chain has been reset\033[0m"
        mkdir -p "$data_dir/bak"
        mv "$node_dir/identity.json" \
           "$client_dir/public key hashs" \
           "$client_dir/public keys" \
           "$client_dir/secret keys" \
           "$data_dir/bak"
        rm -rf "$node_dir" "$client_dir"
        mkdir -p "$node_dir" "$client_dir"
        mv "$data_dir/bak/identity.json" "$node_dir/"
        mv "$data_dir/bak/public key hashs" "$client_dir/"
        mv "$data_dir/bak/public keys" "$client_dir/"
        mv "$data_dir/bak/secret keys" "$client_dir/"
        rmdir "$data_dir/bak"
        cp ~/scripts/alphanet_version "$data_dir/alphanet_version"
    fi
    if [ ! -f "$node_dir/config.json" ]; then
        "$node" config init \
                --data-dir "$node_dir" \
                --rpc-addr "127.0.0.1:8732" \
                --log-output "$node_dir/log" \
                "$@"
    else
        "$node" config update \
                --data-dir "$node_dir" \
                "$@"
    fi
    if [ -f "$node_dir/identity.json" ]; then
        if ! "$node" identity check \
                --data-dir "$node_dir" 2> /dev/null; then
            echo "Ignoring the current peer identity (not enough proof of work)."
            rm "$node_dir/identity.json"
        fi
    fi
    if [ ! -f "$node_dir/identity.json" ]; then
        "$node" identity generate \
                --data-dir "$node_dir"
    fi
}

check_node() {
    pgrep -x tezos-node > /dev/null 2>&1
}

run_node() {
    TEZOS_LOG="${TEZOS_LOG:='* -> info'}"
    if check_node; then
        echo "Cannot run two node instances in the same container."
        exit 1
    fi
    ## Temporary hack until Pierre has debugged Lwt...
    export LWT_ASYNC_METHOD=none
    ## END of temporary hack
    exec "$node" run --data-dir "$node_dir" "$@"
}

stop_node() {
    if ! check_node; then
        echo "No tezos node to kill!"
        exit 1 ;
    fi
    printf "Killing the tezos node..."
    pkill -x tezos-node
    sleep 1
    while check_node; do
        printf "."
        sleep 1
    done
    echo " done."
}

log_node() {
    if ! check_node ; then
        echo
        echo "#############################################"
        echo "##                                         ##"
        echo "## Warning: The tezos node is not running! ##"
        echo "##                                         ##"
        echo "#############################################"
        echo
        tail "$node_dir/log"
    else
        tail -f "$node_dir/log"
    fi
}

run_client() {
    exec $client "$@"
}
