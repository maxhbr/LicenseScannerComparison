#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
#     gitDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/_fossa-cli.git"
#     [[ -d $gitDir ]] || git clone https://github.com/fossas/fossa-cli $gitDir
#     cd $gitDir
#     # git checkout 9447e2d0f63306af8e9da5fc5b4e54d6ae5d5552

#     # TODO: docker image does not build
#     docker_build_here

#     docker_build_stdin <<EOF
# FROM $imageName
# RUN $GOPATH/src/github.com/fossas/fossa-cli/install.sh
# # ENTRYPOINT ["/usr/bin/bash"]
# # CMD []
# EOF

    docker_build_stdin <<EOF
FROM debian:jessie
RUN apt-get update && apt-get install -y curl
RUN groupadd -r user && useradd --no-log-init -r -g user user
RUN curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install.sh | bash \
 && /usr/local/bin/fossa init
ENTRYPOINT ["/usr/local/bin/fossa"]
USER user
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
