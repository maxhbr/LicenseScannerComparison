#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
docker="$(docker info &> /dev/null || echo "sudo") docker"


build() {
    $docker build -t nexb-scancode-toolkit --rm=true --force-rm=true - <<'EOF'
FROM python:3.6.2-jessie

RUN apt-get install -y libbz2-1.0 xz-utils zlib1g libxml2-dev libxslt1-dev
RUN groupadd -r user && useradd --no-log-init -r -m -g user user
VOLUME /home/user/
WORKDIR /scancode-toolkit
RUN chown user:user /scancode-toolkit
USER user
RUN curl -L https://github.com/nexB/scancode-toolkit/archive/v2.9.2.tar.gz | tar xz --strip 1
RUN set -x \
 && echo '#!/usr/bin/env bash' >/scancode-toolkit/entrypoint.sh \
 && echo 'exec /scancode-toolkit/scancode $@' >>/scancode-toolkit/entrypoint.sh \
 && chmod +x /scancode-toolkit/entrypoint.sh


ENTRYPOINT ["/scancode-toolkit/entrypoint.sh"]
CMD ["--help"]
EOF
}

run() {
    $docker rm \
            --force nexb-scancode-toolkit \
            >/dev/null 2>&1 || true

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=nexb-scancode-toolkit \
                -v "$workdir:/toScan" \
                nexb-scancode-toolkit -l /toScan --csv /dev/stdout
    else
        $docker run \
                --name=nexb-scancode-toolkit \
                nexb-scancode-toolkit "$@"
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
