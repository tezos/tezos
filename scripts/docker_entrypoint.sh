#! /usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

data_dir="/var/run/tezos"
export node_dir="$data_dir/node"
export client_dir="$data_dir/client"
export node="tezos-node"
export client="tezos-client -base-dir $client_dir"

. tezos-init-sandboxed-client.sh
. "${script_dir}"/docker_entrypoint.inc.sh

usage() {
    echo "$0 command [options]"
    echo
    echo "where 'command' is one of:"
    echo "  - run_node: ... "
    echo "  - log_node: ... "
    echo "  - run_baker: ... "
    echo "  - log_baker: ... "
    echo "  - run_endorser: ... "
    echo "  - log_endorser: ... "
}

command="$1"
shift

case "$command" in
    init)
        init "$@"
        ;;
    run_node)
        run_node "$@"
        ;;
    check_node)
        check_node
        ;;
    wait_node)
        if ! check_node; then
            echo "The tezos node is not running!"
            exit 1
        fi
        wait_for_the_node_to_be_bootstraped
        may_create_identity
        ;;
    stop_node)
        stop_node
        ;;
    log_node)
        log_node
        ;;
    run_baker)
        run_baker "$@"
        ;;
    check_baker)
        check_baker
        ;;
    stop_baker)
        stop_baker
        ;;
    log_baker)
        log_baker
        ;;
    run_endorser)
        run_endorser "$@"
        ;;
    check_endorser)
        check_endorser
        ;;
    stop_endorser)
        stop_endorser
        ;;
    log_endorser)
        log_endorser
        ;;
    client)
        run_client "$@"
        ;;
    *)
        usage
        exit 1
esac
