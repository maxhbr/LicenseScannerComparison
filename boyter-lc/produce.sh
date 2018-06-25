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
    docker_run --format json $input > $outputDir/output.json
    # also generate other output formats:
    docker_run --format csv $input > $outputDir/output.csv
    docker_run --format spdx $input > $outputDir/output.spdx

    ppJson "$outputDir/output.json"

    "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/transformer.hs "$outputDir/output.json" "$(calculateResultPathFromOutputPath $outputDir)"
}

test $@
