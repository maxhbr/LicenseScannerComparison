#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    gitDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/_ninka.git"
    [[ -d $gitDir ]] || git clone https://github.com/dmgerman/ninka $gitDir
    cd $gitDir

    docker_build_here

    docker_build_stdin <<EOF
FROM $imageName
RUN cpanm --quiet --notest IO::CaptureOutput \
 && rm -fr /root/.cpanm/work
RUN groupadd -r user && useradd --no-log-init -r -m -g user user
USER user
CMD ["--help"]
EOF
}

run() {
    docker_rm

    if [[ -d "$1" ]]; then
        echo "this scanner [$name] only works on files, not folders"
        exit 1
    fi

    docker_run $@
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
