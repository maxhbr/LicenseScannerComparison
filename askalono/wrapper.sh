#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<'EOF'
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
    docker_rm

    if [[ -d "$1" ]]; then
        docker_run_with_toScan crawl $1
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
