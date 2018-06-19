# Scanner:
- Fossology nomos: https://github.com/fossology/fossology/wiki/Nomos
- Fossology monk: https://github.com/fossology/fossology/wiki/Monk
- Scancode https://github.com/nexB/scancode-toolkit
- Triple check https://github.com/pombredanne/triplecheck-engine 
  - Docu: http://triplecheck.tech/index.html
- Open Source License Checker https://sourceforge.net/projects/oslc/
- Ninka http://ninka.turingmachine.org
  - binary: http://ninka.turingmachine.org/download/ninka-1.3.tar.bz2
- Askalono https://github.com/amzn/askalono
- LicenseFinder https://github.com/pivotal-legacy/LicenseFinder
  - needs to be checked because it seems like it is working on metadata
- Slic https://github.com/gerv/slic
- Debian licensecheck https://manpages.debian.org/jessie/devscripts/licensecheck.1.de.html
- Code Aurora lid https://github.com/codeauroraforum/lid
  - equals: https://source.codeaurora.org/external/qostg/lid ?
- licensechecker (lc) https://github.com/boyter/lc
- Licensee https://github.com/benbalter/licensee
- google/licenseclassifier https://github.com/google/licenseclassifier
- go-license-detector https://github.com/src-d/go-license-detector
  - blog: https://blog.sourced.tech/post/gld/

## License Statement Scanner

| Name                     | Scanns for / via        | Wrapped | Produce          | Pinned |
|--------------------------|-------------------------|---------|------------------|--------|
| askalono                 | Top level license files | X       | X                |        |
| benbalter-licensee       |                         | X       | X                |        |
| boyter-lc                |                         | X       | X                |        |
| codeauroraforum-lid      |                         | X       | X                |        |
| debian-licensecheck      |                         | X       | X                |        |
| ninka                    | Scanns single file      | X       | single file only |        |
| gerv-slic                |                         | X       | X                |        |
| go-license-detector      | Top level license files | X       | X                |        |
| google-licenseclassifier | Scanns single file      | X       | single file only |        |
| nexB-scancode-toolkit    |                         | X       | X                |        |

### TODO
- pin scanner versions / build deterministically
  - or preserve the images
- add OutputDataTransformation
#### missing scanners
| Name                    |          |
|-------------------------|----------|
| fossology-monk          |          |
| fossology-nomos         |          |
| oslc                    | java     |
| pivotal-licenseFinder   | legacy   |
| pombredanne-triplecheck | java/ant |

## Code Duplication Scanner
- BDP

## Alternatives (?)
- https://resources.whitesourcesoftware.com/blog-whitesource/still-using-a-scanner-to-identify-your-open-source-it-s-2017-you-can-do-much-better

# Tools
- extractcode https://github.com/nexB/scancode-toolkit/blob/develop/extractcode
