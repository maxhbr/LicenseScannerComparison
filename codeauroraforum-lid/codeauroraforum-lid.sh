#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
docker="$(docker info &> /dev/null || echo "sudo") docker"


build() {
    gitDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/_lid.git"
    [[ -d $gitDir ]] || git clone https://github.com/codeauroraforum/lid $gitDir
    cd $gitDir
    git checkout ea560292f1e89dc81b5a010e06df3b96aff923f5

    $docker build -t codeauroraforum-lid --rm=true --force-rm=true .

    $docker build -t codeauroraforum-lid --rm=true --force-rm=true - <<'EOF'
FROM codeauroraforum-lid


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
    $docker rm \
            --force codeauroraforum-lid \
            >/dev/null 2>&1 || true

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=codeauroraforum-lid \
                -v "$workdir:/toScan" \
                codeauroraforum-lid -S -I /toScan
    else
        $docker run -it \
                --name=codeauroraforum-lid \
                codeauroraforum-lid "$@"
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
