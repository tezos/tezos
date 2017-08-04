#! /bin/sh

set -e

registry_uri="https://registry.gitlab.com/v2"
auth_uri="https://gitlab.com/jwt/auth"

getBearerToken() {
    local headers
    local username
    local reponame
    local basic_token
    username="$1"
    password="$2"
    reponame="$3"
    basic_token=$(echo -n "${username}:${password}" | base64)
    headers="Authorization: Basic ${basic_token}"
    curl -s -H "${headers}" "${auth_uri}?service=container_registry&scope=repository:${reponame}:*" | jq '.token' -r
}

getTagDigest() {
    local token=$1
    local reponame=$2
    local tag=$3
    local id=$(curl -s -H "Authorization: Bearer ${token}" \
                    -D - -o /dev/null \
                    "${registry_uri}/${reponame}/manifests/${tag}" | \
                   grep Docker-Content-Digest | tr -d '\r')
    echo -n "${id##Docker-Content-Digest: }"
}

deleteDigest() {
    local token=$1
    local reponame=$2
    local digest=$3
    curl -s -H "Authorization: Bearer ${token}" \
         -D - -o - -X DELETE \
         "${registry_uri}/${reponame}/manifests/${digest}"
}

deleteTag() {
    local token=$1
    local reponame=$2
    local tag=$3
    local digest="$(getTagDigest "${token}" "${reponame}" "${tag}")"
    if [ -z "$digest" ]; then
        echo "Failed to locate the ${reponame}:${tag}"
        exit 1
    fi
    deleteDigest "${token}" "${reponame}" "${digest}"
}

username="$1"
password="$2"
reponame="$3"
tags="${4:-latest}"

token="$(getBearerToken "${username}" "${password}" "${reponame}")"
if [ -z "$token" ]; then
    echo "Failed to fetch the Bearer token"
    exit 1
fi

for tag in $tags; do
    deleteTag "${token}" "${reponame}" "${tag}"
done
