#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# src: https://github.com/src-d/go-license-detector
# blog: https://blog.sourced.tech/post/gld/
#
# to run:
# $ $0 build
# $ $0 https://github.com/src-d/go-git

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<EOF
FROM golang:1.8
RUN go get -v gopkg.in/src-d/go-license-detector.v2/...
RUN groupadd -r user && useradd --no-log-init -r -g user user
USER user
ENTRYPOINT ["/go/bin/license-detector"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    docker_run $@
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
