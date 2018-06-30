set -e


declare -A SRC
SRC[zlib-1.2.11.tar.gz]=https://zlib.net/zlib-1.2.11.tar.gz
SRC[time-1.7.tar.gz]=http://ftp.gnu.org/gnu/time/time-1.7.tar.gz
SRC[libpng-1.6.34.tar.gz]=https://download.sourceforge.net/libpng/libpng-1.6.34.tar.gz
SRC[file_5.30.tar.xz]=http://http.debian.net/debian/pool/main/f/file/file_5.30.orig.tar.xz
SRC[adol-c-v2.6.3.tar.gz]=https://gitlab.com/adol-c/adol-c/-/archive/v2.6.3/adol-c-v2.6.3.tar.gz
SRC[spdx-testfiles-1.0-for-license-list-2.6.tar]=https://github.com/spdx/license-test-files/raw/master/testfiles-1.0-for-license-list-2.6.tar

datadir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/__data"

calculateSrcDownloadPath() {
    local srcFileName=$1
    echo "$datadir/$srcFileName"
}

calculateSrcPath() {
    local srcFileName=$1
    echo "$(calculateSrcDownloadPath $srcFileName)_extracted"
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
        echo "### already extracted"
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

