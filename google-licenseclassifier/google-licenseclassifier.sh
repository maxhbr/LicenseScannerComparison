#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
docker="$(docker info &> /dev/null || echo "sudo") docker"

build() {
    $docker build -t google-licenseclassifier --rm=true --force-rm=true - <<'EOF'
FROM golang:1.8
RUN groupadd -r user && useradd --no-log-init -r -g user user
WORKDIR /go/src/licenseclassifier
RUN go get -v github.com/google/licenseclassifier/...
USER user
ENTRYPOINT ["identify_license"]
CMD ["--help"]
EOF
}

run() {
    $docker rm \
            --force google-licenseclassifier \
            >/dev/null 2>&1 || true

    if [[ -f "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=google-licenseclassifier \
                -v "$workdir:/toScan" \
                google-licenseclassifier /toScan
    else
        $docker run \
                --name=google-licenseclassifier \
                google-licenseclassifier $@
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
