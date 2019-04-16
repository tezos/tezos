#!/bin/sh

configure_client() {

    local client_config="$HOME/.tezos-client/config"
    mkdir -p "$client_dir" "$HOME/.tezos-client"

    if [ ! -f "$client_config" ]; then
        "$client" --base-dir "$client_dir" \
                  --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
                  config init --output "$client_config" >/dev/null 2>&1
    else
        "$client" --base-dir "$client_dir" \
                  --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
                  config update >/dev/null 2>&1
    fi

}

wait_for_the_node_to_be_ready() {
    local count=0
    if "$client" rpc get /chains/main/blocks/head/hash >/dev/null 2>&1; then return; fi
    printf "Waiting for the node to initialize..."
    sleep 1
    while ! "$client" rpc get /chains/main/blocks/head/hash >/dev/null 2>&1
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
    "$client" bootstrapped
}

launch_node() {

    mkdir -p "$node_dir"

    # Check if we have to reset the chain because the image we want to
    # run has a incompatible version with the blockchain we have stored
    # locally on disk

    local image_version="$(cat "/usr/local/share/tezos/alphanet_version")"
    echo "Current public chain: $image_version."
    local local_data_version=""
    if [ -f "$node_dir/alphanet_version" ]; then
        local_data_version="$(cat "$node_dir/alphanet_version")"
        echo "Local chain data: $local_data_version."
    fi
    if [ "$local_data_version" != "$image_version" ]; then
        echo "Removing outdated chain data..."
        if [ -f "$node_data_dir/identity.json" ]; then \
            mv "$node_data_dir/identity.json" /tmp
        fi
        rm -rf "$node_data_dir"
        rm -rf "$client_dir/"*nonces
        rm -rf "$client_dir/"*endorsements
        rm -rf "$client_dir/"*blocks
        if [ -f "/tmp/identity.json" ]; then \
            mkdir -p "$node_data_dir"
            mv /tmp/identity.json "$node_data_dir/"
        fi
        cp "/usr/local/share/tezos/alphanet_version" \
           "$node_dir/alphanet_version"
    fi

    mkdir -p "$node_data_dir"

    if [ ! -f "$node_data_dir/config.json" ]; then
        echo "Configuring the node..."
        "$node" config init \
                --data-dir "$node_data_dir" \
                --rpc-addr ":$NODE_RPC_PORT" \
                "$@"
    else
        echo "Updating the node configuration..."
        "$node" config update \
                --data-dir "$node_data_dir" \
                --rpc-addr ":$NODE_RPC_PORT" \
                "$@"
    fi

    for i in "$@"; do
        if [ "$i" = "--help" ] ; then exit 0; fi
    done

    # Generate a new identity if not present

    if [ ! -f "$node_data_dir/identity.json" ]; then
        echo "Generating a new node identity..."
        "$node" identity generate "${IDENTITY_POW:-26}". \
                --data-dir "$node_data_dir"
    fi

    configure_client

    # Launching the node

    exec "$node" run --data-dir "$node_data_dir"

}

launch_baker() {
    configure_client
    exec "$baker" --chain main \
	 --base-dir "$client_dir" \
         --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
	 run with local node "$node_data_dir" "$@"
}

launch_baker_test() {
    configure_client
    exec "$baker" --chain test \
	 --base-dir "$client_dir" \
         --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
	 run with local node "$node_data_dir" "$@"
}

launch_endorser() {
    configure_client
    wait_for_the_node_to_be_bootstraped
    exec "$endorser" --chain main \
	 --base-dir "$client_dir" \
         --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
	 run "$@"
}

launch_endorser_test() {
    configure_client
    wait_for_the_node_to_be_bootstraped
    exec "$endorser" --chain test \
	 --base-dir "$client_dir" \
         --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
	 run "$@"
}

launch_accuser() {
    configure_client
    wait_for_the_node_to_be_bootstraped
    exec "$accuser" --base-dir "$client_dir" \
         --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
	 run "$@"
}

launch_accuser_test() {
    configure_client
    wait_for_the_node_to_be_bootstraped
    exec "$accuser" --chain test \
	 --base-dir "$client_dir" \
         --addr "$NODE_HOST" --port "$NODE_RPC_PORT" \
	 run "$@"
}
