#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<'EOF'
FROM python:3.6.2-jessie

RUN apt-get install -y libbz2-1.0 xz-utils zlib1g libxml2-dev libxslt1-dev
RUN groupadd -r user && useradd --no-log-init -r -m -g user user
VOLUME /home/user/
WORKDIR /scancode-toolkit
RUN chown user:user /scancode-toolkit
USER user
RUN curl -L https://github.com/nexB/scancode-toolkit/archive/v2.9.2.tar.gz | tar xz --strip 1
ENTRYPOINT ["/scancode-toolkit/scancode"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -d "$1" ]]; then
        docker_run -l $1 --csv /dev/stdout
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
