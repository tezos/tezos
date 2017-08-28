#!/bin/bash

set -e

if ! docker > /dev/null 2>&1 ; then
    echo "Docker does not seem to be installed."
    exit 1
fi

docker_version="$(docker version -f "{{ .Server.Version }}")"
docker_major="$(echo "$docker_version" | cut -d . -f 1)"
docker_minor="$(echo "$docker_version" | cut -d . -f 2)"

if ([ "$docker_major" -gt 1 ] ||
    ( [ "$docker_major" -eq 1 ] && [ "$docker_minor" -ge 13 ] )) ; then
    docker_1_13=true
else
    docker_1_13=false
fi

current_dir="$(pwd -P)"
src_dir="$(cd "$(dirname "$0")" && echo "$current_dir/")"
cd "$src_dir"

default_port=9732
port="$default_port"

docker_image=docker.io/tezos/tezos:alphanet
docker_volume=tezos-alphanet-data$suffix
suffix=

data_dir="$HOME/.tezos-alphanet$suffix"
docker_container="tezos-alphanet$suffix"

if [ ! -z "$ALPHANET_EMACS" ]; then
    interactive_flags="-t"
else
    interactive_flags="-it"
fi


## Saving state ############################################################

save_identity() {
    if [ ! -f "$data_dir/identity.json" ]; then
        echo "Saving the generated identity into '$data_dir/identity.json'..."
        mkdir -p "$data_dir/"
    fi
    docker cp "$docker_container:var/run/tezos/node/identity.json" \
              "$data_dir/"
}


may_restore_identity() {
    if [ -f "$data_dir/identity.json" ]; then
        echo "Restoring the peer identity from '$data_dir/identity.json'..."
        docker exec "$docker_container" mkdir -p /var/run/tezos/node/
        docker cp "$data_dir/identity.json" \
                  "$docker_container:var/run/tezos/node/"
        docker exec "$docker_container" \
               sudo chown tezos "/var/run/tezos/node/identity.json"
    fi
}

may_save_client_file() {
    if docker exec "$docker_container" \
              test -f "/var/run/tezos/client/$1" ; then
        docker cp "$docker_container:var/run/tezos/client/$1" \
               "$data_dir/$1"
    elif [ -f "$data_dir/$1" ] ; then
        mv "$data_dir/$1" "$data_dir/$1.bak"
    fi
}

may_restore_client_file() {
    if [ -f "$data_dir/$1" ]; then
        docker cp "$data_dir/$1" \
                  "$docker_container:var/run/tezos/client/"
        docker exec "$docker_container" \
               sudo chown tezos "/var/run/tezos/client/$1"
    fi
}

save_accounts() {
    if ! docker exec "$docker_container" \
         test -f "/var/run/tezos/client/secret keys" ; then
        return
    fi
    if [ ! -f "$data_dir/secret keys" ]; then
        echo "Saving the secrets into '$data_dir/secret keys'..."
        echo
        echo -e "\033[33mWARNING: THE SECRET KEYS FILE IS UNENCRYPTED!!!\033[0m"
        echo
        mkdir -p "$data_dir/"
    fi
    docker cp "$docker_container:var/run/tezos/client/secret keys" \
           "$data_dir/"
    may_save_client_file "public key hashs"
    may_save_client_file "public keys"
    may_save_client_file "contracts"
}

may_restore_accounts() {
    docker exec "$docker_container" mkdir -p /var/run/tezos/client/
    if [ -f "$data_dir/secret keys" ]; then
        echo "Restoring the secret keys from '$data_dir/secret keys'..."
        may_restore_client_file "secret keys"
    fi
    may_restore_client_file "public key hashs"
    may_restore_client_file "public keys"
    may_restore_client_file "contracts"
}


## Container ###############################################################

pull_image() {
    if [ "$TEZOS_ALPHANET_DO_NOT_PULL" = "yes" ] ||  [ "$ALPHANET_EMACS" ] ; then
        return ;
    fi
    docker pull "$docker_image"
}

check_container() {
    res=$(docker inspect \
                 --format="{{ .State.Running }}" \
                 --type=container "$docker_container" 2>/dev/null \
              || echo false)
    [ "$res" = true ]
}

check_volume() {
    docker volume inspect "$docker_volume" > /dev/null 2>&1
}

clear_volume() {
    if check_volume ; then
        docker volume rm "$docker_volume" > /dev/null
        echo -e "\033[32mThe blockchain data has been removed from the disk.\033[0m"
    else
        echo -e "\033[32mNo remaining data to be removed from the disk.\033[0m"
    fi
}

uptodate_container() {
    running_image=$(docker inspect \
                           --format="{{ .Image }}" \
                           --type=container "$docker_container")
    latest_image=$(docker inspect \
                          --format="{{ .Id }}" \
                          --type=image "$docker_image")
    [ "$latest_image" = "$running_image" ]
}

assert_container_uptodate() {
    pull_image > /dev/null
    if ! uptodate_container; then
        echo "The current container is not the latest available."
        echo "Please restart."
        exit 1
    fi
}

assert_container() {
    if ! check_container; then
        echo -e "\033[31mNo container currently running!\033[0m"
        exit 1
    fi
}

start_container() {
    if [ "$#" -ge 2 ] && [ "$1" = "--rpc-port" ] ; then
        docker_export_rpc="-p $2:8732"
    fi
    if check_container; then
        assert_container_uptodate
    else
        if ! check_volume; then
            docker volume create "$docker_volume"
        fi
        docker rm "$docker_container" || true > /dev/null 2>&1
        echo "Launching the docker container..."
        docker run --rm -dit -p "$port:$port" $docker_export_rpc \
               -v "$docker_volume:/var/run/tezos" \
               --entrypoint /bin/sh \
               --name "$docker_container" \
               "$docker_image" > /dev/null
        may_restore_identity
        may_restore_accounts
    fi
}

stop_container() {
    if ! check_container; then
        echo -e "\033[31mNo container to kill!\033[0m"
        exit 1
    fi
    save_identity ## Saving again, just in case...
    save_accounts
    printf "Stopping the container... "
    docker stop "$docker_container" >/dev/null
    echo " done"
}


## Node ####################################################################

init_node() {
    docker exec "$docker_container" tezos init \
           "$@" --net-addr "[::]:$port"
    save_identity
}

check_node() {
    check_container && docker exec "$interactive_flags" "$docker_container" tezos check_node
}

assert_node() {
    if ! check_node; then
        echo -e "\033[31mNode is not running!\033[0m"
        exit 0
    fi
}

status_node() {
    if check_node; then
        echo -e "\033[32mNode is running\033[0m"
    else
        echo -e "\033[33mNode is not running\033[0m"
    fi
}

start_node() {
    if check_node; then
        echo -e "\033[31mCannot run two nodes in the same container!\033[0m"
        exit 1
    fi
    if $docker_1_13; then
        tezos_log_env="-eTEZOS_LOG=${TEZOS_LOG:=* -> info}"
    fi
    docker exec -d "${tezos_log_env}" \
           "$docker_container" tezos run_node
    sleep 1
    docker exec "$docker_container" tezos wait_node
    echo -e "\033[32mThe node is now running.\033[0m"
}

log_node() {
    docker exec "$interactive_flags" "$docker_container" tezos log_node
}

stop_node() {
    docker exec "$docker_container" tezos stop_node
}


## Baker ###################################################################

check_baker() {
    check_node && docker exec "$interactive_flags" "$docker_container" tezos check_baker
}

assert_baker() {
    if ! check_baker; then
        echo -e "\033[31mBaker is not running!\033[0m"
        exit 0
    fi
}

status_baker() {
    if check_baker; then
        echo -e "\033[32mBaker is running\033[0m"
    else
        echo -e "\033[33mBaker is not running\033[0m"
    fi
}

start_baker() {
    if check_baker; then
        echo -e "\033[31mCannot run two bakers in the same container!\033[0m"
        exit 1
    fi
    TEZOS_LOG="${TEZOS_LOG:=* -> info}"
    docker exec -d "$docker_container" tezos run_baker
    echo -e "\033[32mThe baker is now running.\033[0m"
}

log_baker() {
    docker exec "$interactive_flags" "$docker_container" tezos log_baker
}

stop_baker() {
    docker exec "$docker_container" tezos stop_baker
}

## Baker ###################################################################

check_endorser() {
    check_node && docker exec "$interactive_flags" "$docker_container" tezos check_endorser
}

assert_endorser() {
    if ! check_baker; then
        echo -e "\033[31mEndorser is not running!\033[0m"
        exit 0
    fi
}

status_endorser() {
    if check_endorser; then
        echo -e "\033[32mEndorser is running\033[0m"
    else
        echo -e "\033[33mEndorser is not running\033[0m"
    fi
}

start_endorser() {
    if check_endorser; then
        echo -e "\033[31mCannot run two endorsers in the same container!\033[0m"
        exit 1
    fi
    TEZOS_LOG="${TEZOS_LOG:=* -> info}"
    docker exec -d "$docker_container" tezos run_endorser
    echo -e "\033[32mThe endorser is now running.\033[0m"
}

log_endorser() {
    docker exec "$interactive_flags" "$docker_container" tezos log_endorser
}

stop_endorser() {
    docker exec "$docker_container" tezos stop_endorser
}

## Misc ####################################################################


run_client() {
    declare -a container_args=();
    for arg in "$@"; do
        if [[ "$arg" == 'container:'* ]]; then
            local_path="${arg#container:}"
            if [[ "$local_path" != '/'* ]]; then
                local_path="$current_dir/$local_path"
            fi
            docker exec "$docker_container" mkdir -p -m 777 /tmp/copied/
            file_name=$(basename "${local_path}")
            docker_path="/tmp/copied/$file_name"
            docker cp "${local_path}" "$docker_container:${docker_path}"
            docker exec "$docker_container" sudo chmod 644 "${docker_path}"
            container_args+=("file:$docker_path");
        else
            container_args+=("${arg}");
        fi
    done
    docker exec "$interactive_flags" "$docker_container" tezos client "${container_args[@]}"
    docker exec "$docker_container" rm -rf /tmp/copied # Remove copied files
    save_accounts
}

run_shell() {
    if [ $# -eq 0 ]; then
        docker exec -it "$docker_container" bash
    else
        docker exec -it "$docker_container" bash -c "$@"
    fi
    save_accounts
}

display_head() {
    docker exec "$interactive_flags" "$docker_container" tezos \
           client rpc call /blocks/head with '{}'
    docker exec "$interactive_flags" "$docker_container" tezos \
           client rpc call /blocks/head/proto/context/level with '{}'
}

## Main ####################################################################

start() {
    pull_image
    start_container "$@"
    init_node "$@"
    start_node
    start_baker
    start_endorser
    save_accounts
    warn_script_uptodate
}

go_alpha_go() {
    docker exec "$interactive_flags" "$docker_container" tezos client \
           activate \
           protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
           with fitness 1 \
           and key dictator
}

stop() {
    stop_node || true
    stop_container
}

status() {
    pull_image
    if ! uptodate_container; then
        echo -e "\033[31mThe container is running but not the latest available.\033[0m"
        exit 1
    fi
    echo -e "\033[32mThe container is running and up to date.\033[0m"
    warn_script_uptodate verbose
    status_node
    status_baker
    status_endorser
}

warn_script_uptodate() {
    if [[ $ALPHANET_EMACS ]]; then
       return
    fi
    docker cp "$docker_container:home/tezos/scripts/alphanet.sh" \
              ".alphanet.sh.new"
    if ! diff .alphanet.sh.new  "$0" >/dev/null 2>&1 ; then
        echo -e "\033[33mWarning: the container contains a new version of 'alphanet.sh'.\033[0m"
        echo -e "\033[33mYou might run '$0 update_script' to synchronize.\033[0m"
    elif [ "$1" = "verbose" ] ; then
        echo -e "\033[32mThe script is up to date.\033[0m"
    fi
    rm .alphanet.sh.new
}

assert_uptodate() {
    assert_container
    assert_container_uptodate
    warn_script_uptodate
}

update_script() {
    pull_image
    tmp="$(docker run --rm -dit --entrypoint /bin/sleep "$docker_image" 20)"
    docker cp "$tmp:home/tezos/scripts/alphanet.sh" ".alphanet.sh.new"
    docker stop "$tmp" > /dev/null
    if ! diff .alphanet.sh.new  "$0" >/dev/null 2>&1 ; then
        mv .alphanet.sh.new "$0"
        echo -e "\033[32mThe script has been updated.\033[0m"
    else
        rm .alphanet.sh.new
        echo -e "\033[32mThe script is up to date.\033[0m"
    fi
}

usage() {
    echo "Usage: $0 [GLOBAL_OPTIONS] <command> [OPTIONS]"
    echo "  Main commands:"
    echo "    $0 start [--rpc-port <int>] [OPTIONS]"
    echo "       Launch a full Tezos alphanet node in a docker container"
    echo "       automatically generating a new network identity."
    echo "       An account 'my_account' for a manager 'my_identity' is also"
    echo "       created to be used via the client."
    echo "       OPTIONS (others than --rpc-port) are directly passed to the"
    echo "       Tezos node, see '$0 shell tezos-node config --help'"
    echo "       for more details."
    echo "       By default, the RPC port is not exported outside the docker"
    echo "       container. WARNING: when exported some RPCs could be harmful"
    echo "       (e.g. 'inject_block', 'force_validation', ...), it is"
    echo "       advised not to export them publicly."
    echo "    $0 <stop|kill>"
    echo "       Friendly or brutally stop the node."
    echo "    $0 restart"
    echo "       Friendly stop the node, fetch the latest docker image and "
    echo "       update this script, then start the node again."
    echo "       The blockchain data are preserved."
    echo "    $0 clear"
    echo "       Remove all the blockchain data from the disk (except"
    echo "       for secret keys and other configuration backup)."
    echo "    $0 status"
    echo "       Check that the running node is running and up to date."
    echo "       Upgrade is automatically done by the start command."
    echo "    $0 head"
    echo "       Display info about the current head of the blockchain."
    echo "    $0 client <COMMAND>"
    echo "       Pass a command to the tezos client."
    echo "    $0 update_script"
    echo "       Replace 'alphanet.sh' with the one found in the docker image."
    echo "  Advanced commands:"
    echo "    $0 container <start|stop|status>"
    echo "    $0 node <start|stop|status|log>"
    echo "    $0 baker <start|stop|status|log>"
    echo "    $0 endorser <start|stop|status|log>"
    echo "    $0 shell"
    echo "Node configuration backup directory: $data_dir"
    echo "Global options are currently limited to:"
    echo "  --port <int>"
    echo "      change public the port Tezos node"
    echo "Container prefix:"
    echo "    container:<FILE>"
    echo "      can be used anywhere 'file:<FILE>' is permitted in client commands."
    echo "      It will cause the referenced file to be copied into the docker conainer."
    echo "      Files will be renamed, which may make errors difficult to read"
}

## Dispatch ################################################################

if [ "$#" -ge 2 ] && [ "$1" = "--port" ] ; then
    port="$2"
    suffix=".$port"
    shift 2
fi

command="$1"
if [ "$#" -eq 0 ] ; then usage ; exit 1;  else shift ; fi


case "$command" in

    ## Main

    start)
        start "$@"
        ;;
    restart)
        if check_container; then
            stop_container
        fi
        update_script
        export TEZOS_ALPHANET_DO_NOT_PULL=yes
        exec "$0" start "$@"
        ;;
    clear)
        if check_container; then
            echo -e "\033[31mCannot clear data while the container is running.\033[0m"
            exit 1
        fi
        clear_volume
        ;;
    status)
        assert_container
        status
        ;;
    stop)
        assert_container
        stop
        ;;
    kill)
        stop_container
        ;;

    ## Container

    container)
        subcommand="$1"
        if [ "$#" -eq 0 ] ; then usage ; exit 1 ; else shift ; fi
        case "$subcommand" in
            start)
                start_container "$@"
                warn_script_uptodate
                ;;
            status)
                if check_container; then
                    echo -e "\033[32mContainer is running\033[0m"
                else
                    echo -e "\033[33mContainer is not running\033[0m"
                fi
                ;;
            stop)
                stop_container
                ;;
            *)
                usage
                exit 1
        esac ;;

    ## Node

    node)
        subcommand="$1"
        if [ "$#" -eq 0 ] ; then usage ; exit 1;  else shift ; fi
        case "$subcommand" in
            start)
                assert_uptodate
                start_node
                ;;
            status)
                assert_uptodate
                status_node
                ;;
            log)
                assert_uptodate
                log_node
                ;;
            stop)
                assert_uptodate
                stop_node
                ;;
            *)
                usage
                exit 1
        esac ;;
    ## Baker

    baker)
        subcommand="$1"
        if [ "$#" -eq 0 ] ; then usage ; exit 1;  else shift ; fi
        case "$subcommand" in
            status)
                assert_uptodate
                status_baker
                ;;
            start)
                assert_uptodate
                assert_node
                start_baker
                ;;
            log)
                assert_uptodate
                log_baker
                ;;
            stop)
                assert_uptodate
                stop_baker
                ;;
            *)
                usage
                exit 1
        esac ;;

    ## Endorser

    endorser)
        subcommand="$1"
        if [ "$#" -eq 0 ] ; then usage ; exit 1;  else shift ; fi
        case "$subcommand" in
            status)
                assert_uptodate
                status_endorser
                ;;
            start)
                assert_uptodate
                assert_node
                start_endorser
                ;;
            log)
                assert_uptodate
                log_endorser
                ;;
            stop)
                assert_uptodate
                stop_endorser
                ;;
            *)
                usage
                exit 1
        esac ;;

    ## Misc.

    head)
        assert_uptodate
        assert_node
        display_head
        ;;
    go_alpha_go)
        assert_uptodate
        assert_node
        go_alpha_go
        ;;
    shell)
        assert_uptodate
        run_shell "$@"
        ;;
    client)
        assert_uptodate
        assert_node
        run_client "$@"
        ;;
    check_script)
        assert_uptodate
        ;;
    update_script)
        update_script
        ;;
    *)
        usage
        exit 1
        ;;
esac
