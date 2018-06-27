#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    gitDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/__lid.git"
    [[ -d $gitDir ]] || git clone https://github.com/codeauroraforum/lid $gitDir
    cd $gitDir
    git checkout ea560292f1e89dc81b5a010e06df3b96aff923f5

    docker_build_here

    docker_build_stdin <<EOF
FROM $imageName

RUN groupadd -r user && useradd --no-log-init -r -g user user
WORKDIR /src
RUN set -x \
 && make deps
#  && make update-licenses \
#  && make pickle
RUN pip install --process-dependency-links /src
USER user
ENTRYPOINT ["license-identifier"]
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -e "$1" ]]; then
        docker_run -S -I $1
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
