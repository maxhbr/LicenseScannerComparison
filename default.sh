#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/common.sh"
. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/testdata.sh"

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

# pull all testdata
pullAll() {
    mkdir -p $datadir
    for srcName in "${!SRC[@]}"; do
        echo "######################################################################"
        echo "## handle $srcName"
        downloadTestdata $srcName "${SRC[$srcName]}"
        extractDownloadedTestdata $srcName
    done
}

# build all images
build() {
    for scanner in "${scanners[@]}"; do
        wrapper="$ROOT/$scanner/wrapper.sh"
        if [[ -x "$wrapper" ]]; then
            $wrapper build
        fi
    done
}

# for each source produce the output for each scanner
produce() {
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
                time="$(time ( $produce $extractedArchive $outputPath > $stdoutFilePath  2> $stderrFilePath || echo "#### failed" ) 2>&1 1>/dev/null )"
                echo $time | tee "$outputPath/time"
                removeEmptyFiles $outputPath
            fi
        done
    done
}

if [ $# -eq 0 ]; then
    pullAll
    build
    produce
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 1
    $@
fi
