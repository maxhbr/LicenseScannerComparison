#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<'EOF'
FROM ruby:2.5
RUN set -x \
 && groupadd -r user && useradd --no-log-init -r -g user user \
 && apt-get update \
 && apt-get install -y cmake \
 && gem install licensee
USER user
ENTRYPOINT ["licensee"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -e "$1" ]]; then
        docker_run detect $1
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
