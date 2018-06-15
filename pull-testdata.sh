#!/usr/bin/env bash
set -e

declare -A SRC
SRC[zlib-1.2.11.tar.gz]=https://zlib.net/zlib-1.2.11.tar.gz
SRC[time-1.7.tar.gz]=http://ftp.gnu.org/gnu/time/time-1.7.tar.gz
SRC[libpng-1.6.34.tar.gz]=https://download.sourceforge.net/libpng/libpng-1.6.34.tar.gz

extractDownloadedTestdata() {
    archive=$1

    extractedArchive="${archive}_extracted"
    if [[ ! -d "$extractedArchive" ]]; then
        echo "### extract ..."
        mkdir -p $extractedArchive

        opt=$( tr '[:upper:]' '[:lower:]' <<<"$archive" )
        case $opt in
            *.tar.gz)
                (cd "$extractedArchive"; tar --strip 1 -xzf -) < "$archive"
                ;;
            *.tar.xz)
                (cd "$extractedArchive"; tar --strip 1 -xzf -) < "$archive"
                ;;
            *)
                echo "#### not supported filetype $archive"
                rmdir $extractedArchive
                ;;
        esac
    else
        echo "### already extracted"
    fi
}

downloadTestdata() {
    archive=$1
    srcUrl=$2

    echo "### download ..."
    wget -q -nc -O $archive $srcUrl || echo "### already downloaded"
    extractDownloadedTestdata $archive
}

pullAll() {
    datadir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/_data"
    mkdir -p $datadir

    for name in "${!SRC[@]}"; do
        echo "######################################################################"
        echo "## handle $name"
        archive="$datadir/$name"
        downloadTestdata $archive "${SRC[$name]}"
    done
}

[[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo "length(\$SRC)=${#SRC[@]}" || pullAll

