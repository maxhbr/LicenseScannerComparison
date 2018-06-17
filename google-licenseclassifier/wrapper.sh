#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<EOF
FROM golang:1.8
RUN groupadd -r user && useradd --no-log-init -r -g user user
RUN go get -v github.com/google/licenseclassifier/...
USER user
ENTRYPOINT ["identify_license"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -d "$1" ]]; then
        echo "this scanner [$name] only works on files, not folders"
        exit 1
    fi

    docker_run $@
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
