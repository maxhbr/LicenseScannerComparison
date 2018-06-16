#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

docker="$(docker info &> /dev/null || echo "sudo") docker"

build() {
    $docker build -t askalono --rm=true --force-rm=true - <<'EOF'
FROM debian:jessie

RUN apt-get update && apt-get install -y wget
RUN groupadd -r user && useradd --no-log-init -r -m -g user user
WORKDIR /askalono
RUN wget https://github.com/amzn/askalono/releases/download/0.2.0/askalono.linux \
 && chmod +x /askalono/askalono.linux

USER user
ENTRYPOINT ["/askalono/askalono.linux"]
CMD ["--help"]
EOF
}

run() {
    $docker rm \
            --force askalono \
            >/dev/null 2>&1 || true

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=askalono \
                -v "$workdir:/toScan" \
                askalono crawl /toScan
    else
        $docker run \
                --name=askalono \
                askalono "$@"
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
