#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    gitDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/_fossa-cli.git"
    [[ -d $gitDir ]] || git clone https://github.com/fossas/fossa-cli $gitDir
    cd $gitDir
    git checkout 9447e2d0f63306af8e9da5fc5b4e54d6ae5d5552

    docker_build_here

    docker_build_stdin <<EOF
FROM $imageName
RUN groupadd -r user && useradd --no-log-init -r -g user user
USER user
ENTRYPOINT ["fossa"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -e "$1" ]]; then
        docker_run $1
    else
        docker_run $@
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
