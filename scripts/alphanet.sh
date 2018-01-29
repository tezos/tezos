#! /usr/bin/env bash

set -e

if ! which docker > /dev/null 2>&1 ; then
    echo "Docker does not seem to be installed."
    exit 1
fi

if ! which docker-compose > /dev/null 2>&1 ; then
    echo "Docker-compose does not seem to be installed."
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

update_compose_file() {

    if [ "$#" -ge 2 ] && [ "$1" = "--rpc-port" ] ; then
        export_rpc="
      - \"$1:8732\""
        shift 2
    fi

    cat > "$docker_compose_yml" <<EOF
version: "3"
services:

  node:
    image: $docker_image
    hostname: node
    command: tezos-node $@
    ports:
      - "$port:$port"$export_rpc
    expose:
      - "8732"
    volumes:
      - node_data:/var/run/tezos/node
      - client_data:/var/run/tezos/client
    restart: on-failure
    environment:
      - P2P_PORT=$port

  baker:
    image: $docker_image
    hostname: baker
    command: tezos-baker --max-priority 128
    links:
      - node
    volumes:
      - client_data:/var/run/tezos/client
    restart: on-failure

  endorser:
    image: $docker_image
    hostname: endorser
    command: tezos-endorser
    links:
      - node
    volumes:
      - client_data:/var/run/tezos/client
    restart: on-failure

volumes:
  node_data:
  client_data:
EOF

}

call_docker_compose() {
    docker-compose -f "$docker_compose_yml" -p "$docker_compose_name" "$@"
}

exec_docker() {
    if [ -t 0 ] && [ -t 1 ] && [ -t 2 ] && [ -z "$ALPHANET_EMACS" ]; then
        local interactive_flags="-it"
    else
        local interactive_flags="-t"
    fi
    docker exec "$interactive_flags" "$docker_node_container" "$@"
}

## Container ###############################################################

pull_image() {
    if [ "$TEZOS_ALPHANET_DO_NOT_PULL" = "yes" ] \
           || [ "$ALPHANET_EMACS" ] \
           || [ "$docker_image" = "$(echo $docker_image | tr -d '/')" ] ; then
        return ;
    fi
    docker pull "$docker_image"
    date "+%s" > "$docker_pull_timestamp"
}

may_pull_image() {
    if [ ! -f "$docker_pull_timestamp" ] \
         || [ 3600 -le $(($(date "+%s") - $(cat $docker_pull_timestamp))) ]; then
        pull_image
    fi
}

uptodate_container() {
    running_image=$(docker inspect \
                           --format="{{ .Image }}" \
                           --type=container "$1")
    latest_image=$(docker inspect \
                          --format="{{ .Id }}" \
                          --type=image "$docker_image")
    [ "$latest_image" = "$running_image" ]
}

assert_container() {
    call_docker_compose up --no-start
}

## Node ####################################################################

check_node_volume() {
    docker volume inspect "$docker_node_volume" > /dev/null 2>&1
}

clear_node_volume() {
    if check_node; then
        echo -e "\033[31mCannot clear data while the node is running.\033[0m"
        exit 1
    fi
    if check_node_volume ; then
        docker volume rm "$docker_node_volume" > /dev/null
        echo -e "\033[32mThe chain data has been removed from the disk.\033[0m"
    else
        echo -e "\033[32mNo remaining data to be removed from the disk.\033[0m"
    fi
}

check_node() {
    res=$(docker inspect \
                 --format="{{ .State.Running }}" \
                 --type=container "$docker_node_container" 2>/dev/null \
              || echo false)
    [ "$res" = true ]
}

assert_node() {
    if ! check_node; then
        echo -e "\033[31mNode is not running!\033[0m"
        exit 0
    fi
}

warn_node_uptodate() {
    if ! uptodate_container "$docker_node_container"; then
        echo -e "\033[33mThe current node is not the latest available.\033[0m"
    fi
}

assert_node_uptodate() {
    may_pull_image
    assert_node
    if ! uptodate_container "$docker_node_container"; then
        echo -e "\033[33mThe current node is not the latest available.\033[0m"
        exit 1
    fi
}

status_node() {
    may_pull_image
    if check_node; then
        echo -e "\033[32mNode is running\033[0m"
        warn_node_uptodate
    else
        echo -e "\033[33mNode is not running\033[0m"
    fi
}

start_node() {
    pull_image
    if check_node; then
        echo -e "\033[31mNode is already running\033[0m"
        exit 1
    fi
    update_compose_file "$@"
    call_docker_compose up --no-start
    call_docker_compose start node
    echo -e "\033[32mThe node is now running.\033[0m"
}

log_node() {
    may_pull_image
    assert_node_uptodate
    call_docker_compose logs -f node
}

stop_node() {
    if ! check_node; then
        echo -e "\033[31mNo node to kill!\033[0m"
        exit 1
    fi
    echo -e "\033[32mStopping the node...\033[0m"
    call_docker_compose stop node
}


## Baker ###################################################################

check_baker() {
    res=$(docker inspect \
                 --format="{{ .State.Running }}" \
                 --type=container "$docker_baker_container" 2>/dev/null \
              || echo false)
    [ "$res" = true ]
}

assert_baker() {
    if ! check_baker; then
        echo -e "\033[31mBaker is not running!\033[0m"
        exit 0
    fi
}

warn_baker_uptodate() {
    if ! uptodate_container "$docker_baker_container"; then
        echo -e "\033[33mThe current baker is not the latest available.\033[0m"
    fi
}

assert_baker_uptodate() {
    assert_baker
    if ! uptodate_container "$docker_baker_container"; then
        echo -e "\033[33mThe current baker is not the latest available.\033[0m"
        exit 1
    fi
}

status_baker() {
    if check_baker; then
        echo -e "\033[32mBaker is running\033[0m"
        may_pull_image
        warn_baker_uptodate
    else
        echo -e "\033[33mBaker is not running\033[0m"
    fi
}

start_baker() {
    if check_baker; then
        echo -e "\033[31mBaker is already running\033[0m"
        exit 1
    fi
    pull_image
    assert_node_uptodate
    call_docker_compose start baker
    echo -e "\033[32mThe baker is now running.\033[0m"
}

log_baker() {
    may_pull_image
    assert_baker_uptodate
    call_docker_compose logs -f baker
}

stop_baker() {
    if ! check_baker; then
        echo -e "\033[31mNo baker to kill!\033[0m"
        exit 1
    fi
    echo -e "\033[32mStopping the baker...\033[0m"
    call_docker_compose stop baker
}

## Endorser ###################################################################

check_endorser() {
    res=$(docker inspect \
                 --format="{{ .State.Running }}" \
                 --type=container "$docker_endorser_container" 2>/dev/null \
              || echo false)
    [ "$res" = true ]
}

assert_endorser() {
    if ! check_endorser; then
        echo -e "\033[31mEndorser is not running!\033[0m"
        exit 0
    fi
}

warn_endorser_uptodate() {
    if ! uptodate_container "$docker_endorser_container"; then
        echo -e "\033[33mThe current endorser is not the latest available.\033[0m"
    fi
}

assert_endorser_uptodate() {
    assert_endorser
    if ! uptodate_container "$docker_endorser_container"; then
        echo -e "\033[33mThe current endorser is not the latest available.\033[0m"
        exit 1
    fi
}

status_endorser() {
    if check_endorser; then
        echo -e "\033[32mEndorser is running\033[0m"
        may_pull_image
        warn_endorser_uptodate
    else
        echo -e "\033[33mEndorser is not running\033[0m"
    fi
}

start_endorser() {
    if check_endorser; then
        echo -e "\033[31mEndorser is already running\033[0m"
        exit 1
    fi
    pull_image
    assert_node_uptodate
    call_docker_compose start endorser
    echo -e "\033[32mThe endorser is now running.\033[0m"
}

log_endorser() {
    may_pull_image
    assert_endorser_uptodate
    call_docker_compose logs -f endorser
}

stop_endorser() {
    if ! check_baker; then
        echo -e "\033[31mNo baker to kill!\033[0m"
        exit 1
    fi
    echo -e "\033[32mStopping the baker...\033[0m"
    call_docker_compose stop endorser
}

## Misc ####################################################################


run_client() {
    assert_node_uptodate
    declare -a container_args=();
    tmpdir=$(exec_docker mktemp)
    for arg in "$@"; do
        if [[ "$arg" == 'container:'* ]]; then
            local_path="${arg#container:}"
            if [[ "$local_path" != '/'* ]]; then
                local_path="$current_dir/$local_path"
            fi
            docker exec "$docker_container" mkdir -p -m 777 "$tmpdir"
            file_name=$(basename "${local_path}")
            docker_path="$tmpdir/$file_name"
            docker cp "${local_path}" "$docker_node_container:${docker_path}"
            exec_docker chmod 644 "${docker_path}"
            container_args+=("file:$docker_path");
        else
            container_args+=("${arg}");
        fi
    done
    exec_docker tezos-client "${container_args[@]}"
    exec_docker rm -rf $tmpdir # Remove copied files
}

run_shell() {
    assert_node_uptodate
    if [ $# -eq 0 ]; then
        exec_docker /bin/sh
    else
        exec_docker /bin/sh -c "$@"
    fi
}

display_head() {
    assert_node_uptodate
    exec_docker tezos-client rpc call /blocks/head with '{}'
    exec_docker tezos-client rpc call /blocks/head/proto/context/level with '{}'
}

## Main ####################################################################

start() {
    pull_image
    update_compose_file "$@"
    call_docker_compose up -d
    warn_script_uptodate
}

stop() {
    call_docker_compose down
}

kill_() {
    call_docker_compose kill
    stop
}

status() {
    status_node
    status_baker
    status_endorser
    warn_script_uptodate verbose
}

warn_script_uptodate() {
    if [[ $ALPHANET_EMACS ]]; then
       return
    fi
    docker run --entrypoint /bin/cat "$docker_image" \
       "/usr/local/share/tezos/alphanet.sh" > ".alphanet.sh.new"
    if ! diff .alphanet.sh.new  "$0" >/dev/null 2>&1 ; then
        echo -e "\033[33mWarning: the container contains a new version of 'alphanet.sh'.\033[0m"
        echo -e "\033[33mYou might run '$0 update_script' to synchronize.\033[0m"
    elif [ "$1" = "verbose" ] ; then
        echo -e "\033[32mThe script is up to date.\033[0m"
    fi
    rm .alphanet.sh.new
}

update_script() {
    docker run --entrypoint /bin/cat "$docker_image" \
       "/usr/local/share/tezos/alphanet.sh" > ".alphanet.sh.new"
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
    suffix="$port"
    shift 2
fi

command="$1"
if [ "$#" -eq 0 ] ; then usage ; exit 1;  else shift ; fi

case $(basename $0) in
    localnet.sh)
        docker_base_dir="$HOME/.tezos-localnet"
        docker_image=tezos:latest
        docker_compose_base_name=localnet
        default_port=14732
        ;;
    zeronet.sh)
        docker_base_dir="$HOME/.tezos-zeronet"
        docker_image=tezos/tezos:zeronet
        docker_compose_base_name=zeronet
        default_port=19732
        ;;
    *)
        docker_base_dir="$HOME/.tezos-alphanet"
        docker_image=tezos:latest
        docker_compose_base_name="alphanet"
        default_port=9732
        ;;
esac

if [ -n "$suffix" ] ; then
    mkdir -p "$docker_base_dir"
    echo "$port" > "$docker_base_dir/default_port"
elif [ -f "$docker_base_dir/default_port" ]; then
    port=$(cat "$docker_base_dir/default_port")
    suffix="$port"
else
    port=$default_port
fi

docker_dir="$docker_base_dir$suffix"
docker_compose_yml="$docker_dir/docker-compose.yml"
docker_pull_timestamp="$docker_dir/docker_pull.timestamp"
docker_compose_name="$docker_compose_base_name$suffix"

docker_node_container=${docker_compose_name}_node_1
docker_baker_container=${docker_compose_name}_baker_1
docker_endorser_container=${docker_compose_name}_endorser_1

docker_node_volume=${docker_compose_name}_node_data
docker_client_volume=${docker_compose_name}_client_data

mkdir -p "$docker_dir"

case "$command" in

    ## Main

    start)
        start "$@"
        ;;
    restart)
        stop
        update_script
        export TEZOS_ALPHANET_DO_NOT_PULL=yes
        exec "$0" start "$@"
        ;;
    clear)
        clear_node_volume
        ;;
    status)
        status
        ;;
    stop)
        stop
        ;;
    kill)
        kill_
        ;;

    ## Node

    node)
        subcommand="$1"
        if [ "$#" -eq 0 ] ; then usage ; exit 1;  else shift ; fi
        case "$subcommand" in
            start)
                start_node "$@"
                ;;
            status)
                status_node
                ;;
            log)
                log_node
                ;;
            stop)
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
                status_baker
                ;;
            start)
                start_baker
                ;;
            log)
                log_baker
                ;;
            stop)
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
                status_endorser
                ;;
            start)
                start_endorser
                ;;
            log)
                log_endorser
                ;;
            stop)
                stop_endorser
                ;;
            *)
                usage
                exit 1
        esac ;;

    ## Misc.

    head)
        display_head
        ;;
    shell)
        run_shell "$@"
        ;;
    client)
        run_client "$@"
        ;;
    check_script)
        warn_script_uptodate verbose
        ;;
    update_script)
        update_script
        ;;
    *)
        usage
        exit 1
        ;;
esac
