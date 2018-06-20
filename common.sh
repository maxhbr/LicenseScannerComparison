# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

[[ "${BASH_SOURCE[0]}" != "${0}" ]] || exit 1

################################################################################
## Variables
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
name="$(basename "$( cd "$( dirname "${0}" )" && pwd )")"

scannerNameToImageName() {
    local name=$1
    echo "lsc/${name,,}"
}

imageName=$(scannerNameToImageName $name)

################################################################################
## Docker related
docker="$(docker info &> /dev/null || echo "sudo") docker"

docker_rm() {
    $docker rm \
            --force $name \
            >/dev/null 2>&1 || true
}

docker_build_here() {
    $docker build \
            -t $imageName \
            --rm=true \
            --force-rm=true \
            .
}

docker_build_stdin() {
    $docker build \
            -t $imageName \
            --rm=true \
            --force-rm=true \
            -
}

docker_run() {
    local toScan=""
    local args=()
    local dockerArgs=()
    for arg in "$@"; do
        if [[ -e $arg ]] && [[ "$toScan" != "/dev/"* ]]; then
            outerName="$(readlink -f "$arg")"
            innerName="/$(basename $outerName)"
            innerName="${innerName%_extracted}"
            dockerArgs+=("-v")
            dockerArgs+=("$outerName:$innerName")
            if [[ -d $arg ]]; then
                args+=("$innerName/")
            else
                args+=("$innerName")
            fi
        else
            args+=("$arg")
        fi
    done

    $docker run \
            -it \
            --rm \
            --name=$name \
            "${dockerArgs[@]}" \
            $imageName "${args[@]}"
}

################################################################################
## Run related
outputROOT="$ROOT/_output"
calculateOutputPath() {
    local src=$1
    local scanner=$2

    echo "$outputROOT/$src/$scanner"
}

removeEmptyFiles() {
    local dir=$1
    find $dir -size  0 -print0 | xargs --no-run-if-empty -0 rm --
}

ppJson() {
    json=$1

    python -m json.tool $json > "${json}_pp"
}
