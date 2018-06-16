#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
docker="$(docker info &> /dev/null || echo "sudo") docker"

build() {
    $docker build -t google-licenseclassifier --rm=true --force-rm=true - <<'EOF'
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
    $docker rm \
            --force google-licenseclassifier \
            >/dev/null 2>&1 || true

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=google-licenseclassifier \
                -v "$workdir:/toScan" \
                google-licenseclassifier detect /toScan
    else
        $docker run -it \
                --name=google-licenseclassifier \
                google-licenseclassifier $@
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
