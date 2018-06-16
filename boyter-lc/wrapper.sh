#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
name='boyter-lc'
docker="$(docker info &> /dev/null || echo "sudo") docker"

build() {
    $docker build -t $name --rm=true --force-rm=true - <<'EOF'
FROM golang:1.10
RUN groupadd -r user && useradd --no-log-init -r -g user user
RUN go get -v github.com/boyter/lc/...
USER user
ENTRYPOINT ["lc"]
CMD ["--help"]
EOF
}

run() {
    $docker rm \
            --force $name \
            >/dev/null 2>&1 || true

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=$name \
                -v "$workdir:/toScan" \
                $name /toScan
    else
        $docker run \
                --name=$name \
                $name $@
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
