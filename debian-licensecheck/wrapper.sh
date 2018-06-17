#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<'EOF'
FROM debian:jessie
RUN apt-get update && apt-get install -y devscripts
RUN groupadd -r user && useradd --no-log-init -r -m -g user user
USER user
ENTRYPOINT ["licensecheck"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -e "$1" ]]; then
        docker_run --recursive $1
        # --machine for machine readable output
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
