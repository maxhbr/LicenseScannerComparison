This is a small project, which tries to compare different license scanners, by
- try to find an almost complete list of usable license scanners
- packaging each of them in a custom docker image, exposing a simple API (**TODO**: 5 are missing)
  - **TODO** pin versions of the scanners / commits of corresponding source, to make it reproducible
- download and extract a set of example projects as source code 
  - **TODO** maybe use [extractcode](https://github.com/nexB/scancode-toolkit/blob/develop/extractcode) or [ununpack](https://github.com/fossology/fossology/tree/master/src/ununpack/agent) for recursive extracting?
- generating the output for each scanner
- **TODO** normalize and understand the output for each scanner
  - maybe a simple csv: `filename,findings,comment`, where
    - the filename is absolute, relative to the source root
    - the findings are space separated

    or
  - maybe a simple json
- **TODO** compare the result to a courated reference result
- **TODO** generate images, which visualize the result in some kind of heatmap / chart

# Scanner:

## License Statement Scanner
| Name                                 | Scanns for / via        | Wrapped | Produce | Pinned |
|--------------------------------------|-------------------------|---------|---------|--------|
| [askalono](https://github.com/amzn/askalono)                        | Top level license files | X       | X       |        |
| [benbalter-licensee](https://github.com/benbalter/licensee)              |                         | X       | X       |        |
| [boyter-lc](https://github.com/boyter/lc) ([blog](https://boyter.org/2018/03/licensechecker-command-line-application-identifies-software-license/))           |                         | X       | X       |        |
| [codeauroraforum-lid](https://github.com/codeauroraforum/lid)             |                         | X       | X       |        |
| [debian-licensecheck](https://manpages.debian.org/jessie/devscripts/licensecheck.1.de.html)             |                         | X       | X       |        |
| [fossology-monk](https://github.com/fossology/fossology/wiki/Monk)                  |                         | X       | X       |        |
| [fossology-nomos](https://github.com/fossology/fossology/wiki/Nomos)                 |                         | X       | X       |        |
| [ninka](http://ninka.turingmachine.org) ([binary](http://ninka.turingmachine.org/download/ninka-1.3.tar.bz2))             | Scanns single file      | X       | X       |        |
| [gerv-slic](https://github.com/gerv/slic)                       |                         | X       | X       |        |
| [go-license-detector](https://github.com/src-d/go-license-detector) ([blog](https://blog.sourced.tech/post/gld/)) | Top level license files | X       | X       |        |
| [google-licenseclassifier](https://github.com/google/licenseclassifier)        | Scanns single file      | X       | X       |        |
| [nexB-scancode-toolkit](https://github.com/nexB/scancode-toolkit)           |                         | X       | X       |        |

#### not yet supported / added scanners
| Name                                           |                                                         |
|------------------------------------------------|---------------------------------------------------------|
| [boyter-python-license-checker](https://github.com/boyter/python-license-checker) ([blog](https://boyter.org/2017/05/identify-software-licenses-python-vector-space-search-ngram-keywords/)) | **only POC?**                                           |
| [fossa-cli](https://github.com/fossas/fossa-cli)                                 | **provided Dockerfile does not build** / only metadata / talks to remote server? |
| [oslc](https://sourceforge.net/projects/oslc/)                                      | java / **old (2007)**                                   |
| [pombredanne-triplecheck](https://github.com/pombredanne/triplecheck-engine) ([docu](http://triplecheck.tech/index.html))       | java/ant                                                |

## License Metadata Scanner
- LicenseFinder https://github.com/pivotal-legacy/LicenseFinder

## Code Duplication Scanner
- BDP

## Alternatives (?)
- https://resources.whitesourcesoftware.com/blog-whitesource/still-using-a-scanner-to-identify-your-open-source-it-s-2017-you-can-do-much-better

