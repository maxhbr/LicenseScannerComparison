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
    # "fossa-cli"
    "fossology-nomos"
    "fossology-monk"
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

saveAll() {
    local saveDir="$ROOT/__images"
    mkdir -p $saveDir
    for scanner in "${scanners[@]}"; do
        local outputTar="$saveDir/$scanner.tar"
        echo "## save $scanner"
        if [[ ! -e "$outputTar" ]];  then
            image="$(scannerNameToImageName $scanner)"
            $docker save -o $outputTar $image
        else
            echo "### already done previously"
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
            echo "### with $scanner"
            outputPath="$(calculateOutputPath $srcName $scanner)"

            numberOfAlreadyGeneratedFiles=0
            if [[ -d $outputPath ]]; then
                alreadyGeneratedOutputFiles=("$outputPath/"*)
                numberOfAlreadyGeneratedFiles=${#alreadyGeneratedOutputFiles[@]}
            fi

            if [[ $numberOfAlreadyGeneratedFiles -eq 0 ]]; then
                produce="$ROOT/$scanner/produce.sh"
                if [[ -x "$produce" ]]; then

                    mkdir -p $outputPath

                    stdoutFilePath="$outputPath/raw.stdout"
                    stderrFilePath="$outputPath/raw.stderr"
                    time="$(time ( $produce $extractedArchive $outputPath > $stdoutFilePath  2> $stderrFilePath || echo "#### failed" ) 2>&1 1>/dev/null )"
                    echo $time | tee "$outputPath/time"
                    removeEmptyFiles $outputPath
                fi
            fi

            resultPath="$(calculateResultPath $srcName $scanner)"
            if [[ ! -e "$resultPath" ]]; then
                transform="$ROOT/$scanner/transformer.hs"
                if [[ -x "$transform" ]]; then
                    echo "#### transform"
                    "$transform" "$outputPath" "$resultPath"
                fi
            fi
        done

        listTestdata $srcName > "$(calculateResultFileListPath $srcName)"
    done
}

generate() {
    cat <<EOF > "$resultROOT/index.html"
<!DOCTYPE html>
<html>
<head>
<title>LicenseScannerComparison</title>
</head>
<body>
<ul>
EOF
    for srcName in "${!SRC[@]}"; do
        echo "## generate for $srcName"
        resultDir="$(calculateResultPath $srcName)"
        "$ROOT/generatePage.hs" "$resultDir"

        extractedArchive=$(calculateSrcPath $srcName)
        if [[ ! -e "$resultDir/$srcName" ]]; then
            ln -rs $extractedArchive "$resultDir/$srcName"
        fi

        echo "<li><a href=\"$srcName/index.html\">$srcName</a></li>" >> "$resultROOT/index.html"
    done
    cat <<EOF >> "$resultROOT/index.html"
</ul>
</body>
</html>
EOF
}

if [ $# -eq 0 ]; then
    pullAll
    build
    produce
    generate
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 1
    $@
fi
