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
    touch "$outputDir/output.json" "$outputDir/output.csv" "$outputDir/output.rdf.xml" "$outputDir/output.spdx"
    chmod 666 "$outputDir/output.json" "$outputDir/output.csv" "$outputDir/output.rdf.xml" "$outputDir/output.spdx"
    docker_run \
               --json "$outputDir/output.json" \
               --csv "$outputDir/output.csv" \
               --spdx-rdf "$outputDir/output.rdf.xml" \
               --spdx-tv "$outputDir/output.spdx" \
               -l -i -c $input

    ppJson "$outputDir/output.json"
}

test $@
