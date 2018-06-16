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

| Wrapped | Name                     | License    | Scanns for / via        |
|---------|--------------------------|------------|-------------------------|
|         | nomos                    | GPL-2-only |                         |
|         | monk                     | GPL-2-only |                         |
| X       | go-license-detector      | Apache-2.0 | Top level license files |
| X       | Scancode                 | Apache-2.0 |                         |
| X       | Askalono                 |            | Top level license files |
| X       | Code Aurora lid          |            |                         |
| X       | google/licenseclassifier |            | Scanns single file      |
| X       | benbalter-licensee       |            |                         |

# Tools
- extractcode https://github.com/nexB/scancode-toolkit/blob/develop/extractcode
