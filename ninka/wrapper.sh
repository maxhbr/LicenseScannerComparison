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
RUN set -x \
 && echo '#!/usr/bin/env bash' >/entrypoint.sh \
 && echo 'if [[ -d \$1 ]]; then' >>/entrypoint.sh \
 && echo '    find \$1 -type f -exec /usr/local/bin/ninka {} \\;' >>/entrypoint.sh \
 && echo 'else' >>/entrypoint.sh \
 && echo '    exec /usr/local/bin/ninka \$@' >>/entrypoint.sh \
 && echo 'fi' >>/entrypoint.sh \
 && chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
USER user
CMD ["--help"]
EOF
     # && echo '    find \$1 -type f -exec bash -c \\'echo "{};\\\$(/usr/local/bin/ninka {})"\\' \\;' >>/entrypoint.sh \
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
