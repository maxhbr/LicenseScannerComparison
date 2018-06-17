# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

[[ "${BASH_SOURCE[0]}" != "${0}" ]] || exit 1

################################################################################
## Variables
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
name="$(basename "$( cd "$( dirname "${0}" )" && pwd )")"
imageName="lsc/${name,,}"

################################################################################
## Docker related
docker="$(docker info &> /dev/null || echo "sudo") docker"

docker_rm() {
    $docker rm \
            --force $name \
            >/dev/null 2>&1 || true
}

docker_build_here() {
    $docker build -t $imageName --rm=true --force-rm=true .
}

docker_build_stdin() {
    $docker build -t $imageName --rm=true --force-rm=true -
}

docker_run() {
    local workdir=""
    local array=()
    for arg in "$@"; do
        if [[ -e $arg ]] && [[ "$workdir" == "" ]]; then
            workdir="$(readlink -f "$arg")"
            array+=("/toScan")
        else
            array+=("$arg")
        fi
    done

    if [[ "$workdir" == "" ]]; then
        $docker run -it \
                --name=$name \
                $imageName $@
    else
        $docker run \
                --name=$name \
                -v "$workdir:/toScan" \
                $imageName "${array[@]}"
    fi
}
