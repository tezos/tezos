#! /bin/sh

registry_uri="https://registry.gitlab.com/v2"
auth_uri="https://gitlab.com/jwt/auth"

scope=push,pull

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
    curl -fs -H "${headers}" "${auth_uri}?service=container_registry&scope=repository:${reponame}:${scope}" | jq '.token' -r
}

getTagDigest() {
    local token=$1
    local reponame=$2
    local tag=$3
    local digest="$(curl -fs -H "Authorization: Bearer ${token}" \
                            -H "Accept: application/vnd.docker.distribution.manifest.v2+json" \
                    --head  \
                    "${registry_uri}/${reponame}/manifests/${tag}" | \
                   grep Docker-Content-Digest | tr -d '\r')"
    echo -n "${digest##Docker-Content-Digest: }"
}

deleteDigest() {
    local token=$1
    local reponame=$2
    local digest=$3
    curl -fs -H "Authorization: Bearer ${token}" \
         -D - -o - -X DELETE "${registry_uri}/${reponame}/manifests/${digest}"
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

readTag() {
    local token=$1
    local reponame=$2
    local tag=$3
    curl -fs -H "Authorization: Bearer ${token}" \
         -H "Accept: application/vnd.docker.distribution.manifest.v2+json" \
         --head "$registry_uri/$reponame/manifests/$tag" | \
        grep Docker-Content-Digest
}

renameTag() {
    local token=$1
    local reponame=$2
    local old_tag=$3
    local new_tag=$4
    curl -fs -H "Authorization: Bearer ${token}" \
         -H "Accept: application/vnd.docker.distribution.manifest.v2+json" \
         -o MANIFEST "$registry_uri/$reponame/manifests/$old_tag"
    curl -fs -H "Authorization: Bearer ${token}" \
         -H "Content-type: application/vnd.docker.distribution.manifest.v2+json" \
         -X PUT -d @MANIFEST "$registry_uri/$reponame/manifests/$new_tag"
    rm MANIFEST
}
