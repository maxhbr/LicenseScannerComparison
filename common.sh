# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
################################################################################
## Variables
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
name="lsc/$(basename "$( cd "$( dirname "${0}" )" && pwd )")"

################################################################################
## Docker related
docker="$(docker info &> /dev/null || echo "sudo") docker"

docker_rm() {
    local name=$1

    $docker rm \
            --force $name \
            >/dev/null 2>&1 || true
}
