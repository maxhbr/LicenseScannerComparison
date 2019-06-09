#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
. "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/common.sh"

scanners=(
    "benbalter-licensee"
    "codeauroraforum-lid"
    # "debian-licensecheck"
    # "google-licenseclassifier"
    # "fossa-cli"
    "fossology-nomos"
    "fossology-monk"
    "gerv-slic"
    "boyter-lc"
    "askalono"
    "ninka"
    "nexB-scancode-toolkit"
    "go-license-detector")

declare -A SRC
SRC[zlib-1.2.11.tar.gz]=https://zlib.net/zlib-1.2.11.tar.gz
SRC[time-1.7.tar.gz]=http://ftp.gnu.org/gnu/time/time-1.7.tar.gz
SRC[libpng-1.6.34.tar.gz]=https://download.sourceforge.net/libpng/libpng-1.6.34.tar.gz
SRC[file_5.30.tar.xz]=http://http.debian.net/debian/pool/main/f/file/file_5.30.orig.tar.xz
SRC[adol-c-v2.6.3.tar.gz]=https://gitlab.com/adol-c/adol-c/-/archive/v2.6.3/adol-c-v2.6.3.tar.gz
SRC[spdx-testfiles-1.0-for-license-list-2.6.tar]=https://github.com/spdx/license-test-files/raw/master/testfiles-1.0-for-license-list-2.6.tar
# SRC[thrift-0.12.0.tar.gz]='http://www.apache.org/dyn/closer.cgi?path=/thrift/0.12.0/thrift-0.12.0.tar.gz'

datadir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/__data"

calculateSrcDownloadPath() {
    local srcFileName=$1
    echo "$datadir/$srcFileName"
}

calculateSrcPath() {
    local srcFileName=$1

    srcPath="$(calculateResultPath "$srcFileName")/__/$srcFileName"
    mkdir -p "$(dirname "$srcPath")"
    echo "$srcPath"
}

expectationsdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/_expectations"
calculateExpectationFile() {
    local srcFileName=$1
    possiblePath="$expectationsdir/${srcFileName}_expectations.csv"
    if [[ -f "$possiblePath" ]]; then
        echo "$possiblePath"
    else
        echo "/dev/null"
    fi
}

extractDownloadedTestdata() {
    srcName=$1

    archive=$(calculateSrcDownloadPath $srcName)
    extractedArchive=$(calculateSrcPath $srcName)
    if [[ ! -d "$extractedArchive" ]]; then
        echo "### extract ..."
        mkdir -p $extractedArchive

        opt=$( tr '[:upper:]' '[:lower:]' <<<"$archive" )
        case $opt in
            *.tar.gz)
                (cd "$extractedArchive"; tar --strip 1 -xzf -) < "$archive"
                ;;
            *.tar.xz)
                (cd "$extractedArchive"; tar --strip 1 -xJf -) < "$archive"
                ;;
            *.tar)
                (cd "$extractedArchive"; tar --strip 1 -xf -) < "$archive"
                ;;
            *)
                echo "#### not supported filetype $archive"
                rmdir $extractedArchive
                ;;
        esac
    else
        echo "### $extractedArchive is already extracted"
    fi
}

downloadTestdata() {
    srcName=$1
    srcUrl=$2

    echo "### download ..."
    wget -q -nc -O $(calculateSrcDownloadPath $srcName) $srcUrl || echo "### already downloaded"
}

pullAll() {
    mkdir -p $datadir
    for srcName in "${!SRC[@]}"; do
        echo "######################################################################"
        echo "## handle $srcName"
        downloadTestdata $srcName "${SRC[$srcName]}"
        extractDownloadedTestdata $srcName
    done
}

listTestdata() {
    local srcFileName=$1
    srcDir=$(calculateSrcPath $srcFileName)
    find $srcDir -type f | sed 's%'"$srcDir"'%'"/$srcFileName"'%' | sort
}

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
        expectationFile="$(calculateExpectationFile $srcName)"
        "$ROOT/generatePage.hs" "$resultDir" "$expectationFile"
        "$ROOT/generateMasterJSON.hs" "$resultDir" "$expectationFile"

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
