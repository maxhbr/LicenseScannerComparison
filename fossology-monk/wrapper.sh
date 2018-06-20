#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

build() {
    docker_build_stdin <<EOF
FROM fossology/fossology:latest
RUN set -x\
 && echo 'dbname=fossology;' > /usr/local/etc/fossology/DB.conf \
 && echo 'host=localhost;' >> /usr/local/etc/fossology/DB.conf \
 && echo 'user=fossy;' >> /usr/local/etc/fossology/DB.conf \
 && echo 'password=fossy;' >> /usr/local/etc/fossology/DB.conf \
 && echo '#!/usr/bin/env bash' >/entrypoint.sh \
 && echo '/etc/init.d/postgresql start &>/dev/null' >>/entrypoint.sh \
 && echo '/usr/local/lib/fossology/fo-postinstall &>/dev/null' >>/entrypoint.sh \
 && echo 'if [[ -d \$1 ]]; then' >>/entrypoint.sh \
 && echo '    exec find \$1 -type f -exec echo -n "{};" \\; -exec /fossology/src/monk/agent/monk -J {} \\; -exec echo "" \\; 2>/dev/null' >>/entrypoint.sh \
 && echo 'else' >>/entrypoint.sh \
 && echo '    exec /fossology/src/monk/agent/monk \$@ 2>/dev/null' >>/entrypoint.sh \
 && echo 'fi' >>/entrypoint.sh \
 && chmod +x /entrypoint.sh
# RUN groupadd -r user && useradd --no-log-init -r -g user user
# USER user
ENTRYPOINT ["/entrypoint.sh"]
CMD ["--help"]
EOF
    # TODO: instead of redirecting to /dev/null it should work to redirect to /dev/stderr
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
