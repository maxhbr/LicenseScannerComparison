# Scanner:
- Fossology nomos: https://github.com/fossology/fossology/wiki/Nomos
- Fossology monk: https://github.com/fossology/fossology/wiki/Monk
- Scancode https://github.com/nexB/scancode-toolkit
- Triple check https://github.com/pombredanne/triplecheck-engine 
  - Docu: http://triplecheck.tech/index.html
- Open Source License Checker https://sourceforge.net/projects/oslc/
- Ninka http://ninka.turingmachine.org
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

## Details

| Name                     | Scanns for / via        | Wrapped |
|--------------------------|-------------------------|---------|
| askalono                 | Top level license files | X       |
| benbalter-licensee       |                         | X       |
| boyter-lc                |                         | X       |
| codeauroraforum-lid      |                         | X       |
| debian-licensecheck      |                         | X       |
| gerv-slic                |                         | X       |
| go-license-detector      | Top level license files | X       |
| google-licenseclassifier | Scanns single file      | X       |
| nexB-scancode-toolkit    |                         | X       |

### TODO
| Name                    |          |
|-------------------------|----------|
| fossology-monk          |          |
| fossology-nomos         |          |
| ninka                   |          |
| oslc                    | java     |
| pivotal-licenseFinder   |          |
| pombredanne-triplecheck | java/ant |


# Tools
- extractcode https://github.com/nexB/scancode-toolkit/blob/develop/extractcode
