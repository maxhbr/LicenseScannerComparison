#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

name="benbalter-licensee"

build() {
    $docker build -t $name --rm=true --force-rm=true - <<'EOF'
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
    docker_rm $name

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=$name \
                -v "$workdir:/toScan" \
                $name detect /toScan
    else
        $docker run -it \
                --name=$name \
                $name $@
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
