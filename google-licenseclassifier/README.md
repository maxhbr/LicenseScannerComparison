# Example result:
```
$ ./google-licenseclassifier.sh ../_data/time-1.7.tar.gz_extracted/COPYING
2018/06/16 18:21:22 Classifying license(s): /toScan
2018/06/16 18:21:23 Finished Classifying License "/toScan": 126.675623ms
/toScan: GPL-2.0 (confidence: 0.9966288269693843, offset: 0, extent: 14516)
```

# Generated Output
```
/time-1.7.tar.gz/resuse.h: GPL-1.0 (confidence: 0.9112149532710281, offset: 112, extent: 586)
/time-1.7.tar.gz/resuse.h: GPL-3.0 (confidence: 0.8726655348047538, offset: 83, extent: 573)
/time-1.7.tar.gz/time.c: GPL-1.0 (confidence: 0.9112149532710281, offset: 110, extent: 586)
/time-1.7.tar.gz/time.c: GPL-3.0 (confidence: 0.8726655348047538, offset: 81, extent: 573)
/time-1.7.tar.gz/getopt1.c: GPL-1.0 (confidence: 0.9112149532710281, offset: 132, extent: 586)
/time-1.7.tar.gz/getopt1.c: GPL-3.0 (confidence: 0.8726655348047538, offset: 103, extent: 573)
/time-1.7.tar.gz/resuse.c: GPL-1.0 (confidence: 0.9112149532710281, offset: 95, extent: 586)
/time-1.7.tar.gz/resuse.c: GPL-3.0 (confidence: 0.8726655348047538, offset: 66, extent: 573)
/time-1.7.tar.gz/mdate-sh: GPL-3.0 (confidence: 0.9219015280135824, offset: 161, extent: 544)
/time-1.7.tar.gz/error.c: GPL-1.0 (confidence: 0.9112149532710281, offset: 115, extent: 586)
/time-1.7.tar.gz/error.c: GPL-3.0 (confidence: 0.8726655348047538, offset: 86, extent: 573)
/time-1.7.tar.gz/getopt.h: GPL-1.0 (confidence: 0.9112149532710281, offset: 85, extent: 586)
/time-1.7.tar.gz/getopt.h: GPL-3.0 (confidence: 0.8726655348047538, offset: 56, extent: 573)
/time-1.7.tar.gz/getopt.c: GPL-1.0 (confidence: 0.9112149532710281, offset: 244, extent: 586)
/time-1.7.tar.gz/getopt.c: GPL-3.0 (confidence: 0.8726655348047538, offset: 215, extent: 573)
/time-1.7.tar.gz/COPYING: GPL-2.0 (confidence: 0.9966288269693843, offset: 0, extent: 14516)
/time-1.7.tar.gz/COPYING: GPL-3.0 (confidence: 0.9473684210526316, offset: 15154, extent: 559)
/time-1.7.tar.gz/texinfo.tex: GPL-3.0 (confidence: 0.8488964346349746, offset: 132, extent: 525)
/time-1.7.tar.gz/wait.h: GPL-1.0 (confidence: 0.9112149532710281, offset: 95, extent: 586)
/time-1.7.tar.gz/wait.h: GPL-3.0 (confidence: 0.8726655348047538, offset: 66, extent: 573)
```

## Expected convertet output
```
"resuse.h";"GPL-1.0,GPL-3.0";
"time.c";"GPL-1.0,GPL-3.0";
[...]
```
