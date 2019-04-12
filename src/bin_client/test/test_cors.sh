#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1 --cors-origin "*"

show_logs="no"

sleep 2

run_preflight() {
    local origin="$1"
    local method="$2"
    local cors_method="$3"
    local header="$4"
    curl -H "Origin: $origin" \
         -H "Access-Control-Request-Method: $cors_method" \
         -H "Access-Control-Request-Headers: $header" \
         -X $method \
         -I -s http://localhost:18731/chains/main/blocks/head/header/shell > CURL.$id 2>&1
}

run_request() {
    local origin="$1"
    curl -H "Origin: $origin" \
         -H "Content-Type: application/json" \
         -D CURL.$id \
         -s http://localhost:18731/chains/main/blocks/head/header/shell 2>&1 > /dev/null
}

# Preflight
run_preflight "localhost" "OPTIONS" "GET" "Content-Type"
cat CURL.$id
grep -q "access-control-allow-origin" CURL.$id
grep -q "access-control-allow-methods" CURL.$id
grep -q "access-control-allow-headers" CURL.$id

# Request
run_request "localhost"
cat CURL.$id
grep -q "access-control-allow-origin" CURL.$id

echo
echo End of test
echo
