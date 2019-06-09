# Generated Output
```
$ cat _output/time-1.7.tar.gz/gerv-slic/output.json
[
  {
    "files": [
      "/time-1.7.tar.gz/resuse.h", 
      "/time-1.7.tar.gz/time.c", 
      "/time-1.7.tar.gz/getopt1.c", 
      "/time-1.7.tar.gz/resuse.c", 
      "/time-1.7.tar.gz/mdate-sh", 
      "/time-1.7.tar.gz/error.c", 
      "/time-1.7.tar.gz/getopt.h", 
      "/time-1.7.tar.gz/getopt.c", 
      "/time-1.7.tar.gz/COPYING", 
      "/time-1.7.tar.gz/texinfo.tex", 
      "/time-1.7.tar.gz/wait.h"
    ], 
    "tag": "GPL-2.0+"
  }, 
  {
    "files": [
      "/time-1.7.tar.gz/install-sh"
    ], 
    "tag": "HPND"
  }, 
  {
    "files": [
      "/time-1.7.tar.gz/mkinstalldirs"
    ], 
    "tag": "PD"
  }, 
  {
    "files": [
      "/time-1.7.tar.gz/port.h", 
      "/time-1.7.tar.gz/version.c", 
      "/time-1.7.tar.gz/getpagesize.h", 
      "/time-1.7.tar.gz/Makefile.am", 
      "/time-1.7.tar.gz/version.texi", 
      "/time-1.7.tar.gz/configure.in", 
      "/time-1.7.tar.gz/README"
    ], 
    "tag": "none"
  }, 
  {
    "files": [
      "/time-1.7.tar.gz/time.texi", 
      "/time-1.7.tar.gz/Makefile.in"
    ], 
    "tag": "suspiciousLicensey"
  }
]

```

## Expected convertet output
```
"resuse.h";"GPL-2.0+";
"time.c";"GPL-2.0+";
[...]
"install-sh";"HPND";
[...]
"port.h";"NONE";
[...]
"time.texi";"NOASSERTION";
[...]
```
## Mapping
| from                 | to                |
|----------------------|-------------------|
| `none`               | `NONE`            |
| `suspiciousLicensey` | `NOASSERTION`     |
| `PD`                 | `public domain` ? |
