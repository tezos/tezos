#! /usr/bin/env bash

node_dirs=()
node_pids=()

start_sandboxed_node() {

    id=$1
    max_peer_id=${max_peer_id:-9}
    shift 1

    port=$((19730 + id))
    rpc=$((18730 + id))
    expected_pow="${expected_pow:-0.0}"
    expected_connections="${expected_connections:-3}"
    node_dir="$(mktemp -d -t tezos-node.XXXXXXXX)"
    peers=("--no-bootstrap-peers")
    for peer_port in $(seq 19730 $((19730 + max_peer_id))); do
        peers+=("--peer")
        peers+=("127.0.0.1:$peer_port")
    done
    peers+=("--closed")
    node="$src_dir/tezos-node"
    sandbox_file="${sandbox_file:-$script_dir/sandbox.json}"
    sandbox_param="--sandbox=$sandbox_file"

    node_dirs+=("$node_dir")

    $node config init \
          --data-dir "$node_dir" \
          --net-addr "127.0.0.1:$port" \
          --rpc-addr "127.0.0.1:$rpc" \
          --expected-pow "$expected_pow" \
          --connections "$expected_connections"
    $node identity generate "$expected_pow" --data-dir "$node_dir"
    $node run --data-dir "$node_dir" "${peers[@]}" "$sandbox_param" "$@" &
    node_pids+=("$!")

}

cleanup_nodes() {
    [ -z "${node_pids[0]}" ] || kill "${node_pids[@]}"
    for pid in "${node_pids[@]}" ; do wait "$pid" ; done
    rm -rf "${node_dirs[@]}"
}
