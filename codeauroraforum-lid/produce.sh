#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$(dirname "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )")/common.sh"

test() {
    local input=$1
    local outputDir=$2

    mkdir -p $outputDir
    chmod 777 $outputDir

    docker_rm
    docker_run -I $input -F csv -O "$outputDir/"
}

test $@
