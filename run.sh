#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/common.sh"
. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/pull-testdata.sh"

scanners=(
    "benbalter-licensee"
    "codeauroraforum-lid"
    "debian-licensecheck"
    "google-licenseclassifier"
    "gerv-slic"
    "boyter-lc"
    "askalono"
    "ninka"
    "nexB-scancode-toolkit"
    "go-license-detector")


for srcName in "${!SRC[@]}"; do
    extractedArchive=$(calculateSrcPath $srcName)
    [[ -d "$extractedArchive" ]] || continue

    echo "## do $srcName"
    for scanner in "${scanners[@]}"; do
        outputPath=$(calculateOutputPath $srcName $scanner)

        if [[ -d $outputPath ]]; then
            alreadyGeneratedOutputFiles=("$outputPath/"*)
            [[ ${#alreadyGeneratedOutputFiles[@]} -gt 0 ]] && continue
        fi

        produce="$ROOT/$scanner/produce.sh"
        if [[ -x "$produce" ]]; then
            echo "### with $scanner"

            mkdir -p $outputPath

            stdoutFilePath="$outputPath/raw.stdout"
            stderrFilePath="$outputPath/raw.stderr"
            time $produce $extractedArchive $outputPath \
                 1> >(tee -a $stdoutFilePath) \
                 2> >(tee -a $stderrFilePath)
            removeEmptyFiles $outputPath
        fi
    done
done
