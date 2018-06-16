docker="$(docker info &> /dev/null || echo "sudo") docker"


docker_rm() {
    local name=$1

    $docker rm \
            --force $name \
            >/dev/null 2>&1 || true
}
