#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<'EOF'
FROM golang:1.10
RUN groupadd -r user && useradd --no-log-init -r -g user user
RUN go get -v github.com/boyter/lc/...
USER user
ENTRYPOINT ["lc"]
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
