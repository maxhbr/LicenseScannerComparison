#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<EOF
FROM golang:1.8
RUN groupadd -r user && useradd --no-log-init -r -g user user
RUN go get -v github.com/google/licenseclassifier/...
RUN set -x \
 && echo '#!/usr/bin/env bash' >/entrypoint.sh \
 && echo 'if [[ -d \$1 ]]; then' >>/entrypoint.sh \
 && echo '    find \$1 -type f -exec identify_license -headers {} \\;' >>/entrypoint.sh \
 && echo 'else' >>/entrypoint.sh \
 && echo '    exec identify_license \$@' >>/entrypoint.sh \
 && echo 'fi' >>/entrypoint.sh \
 && chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
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
