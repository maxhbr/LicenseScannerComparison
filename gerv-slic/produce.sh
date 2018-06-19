#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

test() {
    local input=$1
    local outputDir=$2

    mkdir -p $outputDir

    docker_rm
    docker_run $input > $outputDir/output.json
    # detailed output with `-D` fails, since some copyrights or texts are "not serializable"
}

test $@
