#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<'EOF'
FROM python:2.7.15-jessie
RUN groupadd -r user && useradd --no-log-init -r -m -g user user
WORKDIR /slic
RUN chown user:user /slic
USER user
RUN curl -L https://github.com/gerv/slic/archive/master.tar.gz | tar xz --strip 1
ENTRYPOINT ["/slic/slic"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -d "$1" ]]; then
        docker_run -D --plain $1
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
