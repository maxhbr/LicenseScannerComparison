# Results
## on `time-1.7.tar.gz_extracted`
```
$ ./benbalter-licensee.sh ../_data/time-1.7.tar.gz_extracted
License:        GPL-2.0
Matched files:  COPYING
COPYING:
  Content hash:  da72d8786703dfb3f4ccf3f9b600f948bf6d26c2
  Confidence:    98.75%
  Matcher:       Licensee::Matchers::Dice
  License:       GPL-2.0
  Closest licenses:
    GPL-2.0 similarity:   98.75%
    LGPL-2.1 similarity:  79.94%
    GPL-3.0 similarity:   60.38%
```

# Generated Output
```
$ cat _output/time-1.7.tar.gz/benbalter-licensee/output.json | <prettyPrint>
{
    "licenses": [
        {
            "cc": false,
            "fields": [],
            "gpl": true,
            "key": "gpl-2.0",
            "lgpl": false,
            "meta": {
                "description": "The GNU GPL is the most widely used free software license and has a strong copyleft requirement. When distributing derived works, the source code of the work must be made available under the same license. There are multiple variants of the GNU GPL, each with different requirements.",
                "featured": false,
                "hidden": false,
                "how": "Create a text file (typically named LICENSE or LICENSE.txt) in the root of your source code and copy the text of the license into the file.",
                "nickname": "GNU GPLv2",
                "note": "The Free Software Foundation recommends taking the additional step of adding a boilerplate notice to the top of each file. The boilerplate can be found at the end of the license.",
                "source": "https://www.gnu.org/licenses/gpl-2.0.txt",
                "title": "GNU General Public License v2.0",
                "using": [
                    {
                        "AliSQL": "https://github.com/alibaba/AliSQL/blob/master/COPYING"
                    },
                    {
                        "Discourse": "https://github.com/discourse/discourse/blob/master/LICENSE.txt"
                    },
                    {
                        "Joomla!": "https://github.com/joomla/joomla-cms/blob/staging/LICENSE.txt"
                    }
                ]
            },
            "other": false,
            "rules": {
                "conditions": [
                    {
                        "description": "A copy of the license and copyright notice must be included with the software.",
                        "label": "License and copyright notice",
                        "tag": "include-copyright"
                    },
                    {
                        "description": "Changes made to the code must be documented.",
                        "label": "State changes",
                        "tag": "document-changes"
                    },
                    {
                        "description": "Source code must be made available when the software is distributed.",
                        "label": "Disclose source",
                        "tag": "disclose-source"
                    },
                    {
                        "description": "Modifications must be released under the same license when distributing the software. In some cases a similar or related license may be used.",
                        "label": "Same license",
                        "tag": "same-license"
                    }
                ],
                "limitations": [
                    {
                        "description": "This license includes a limitation of liability.",
                        "label": "Liability",
                        "tag": "liability"
                    },
                    {
                        "description": "The license explicitly states that it does NOT provide any warranty.",
                        "label": "Warranty",
                        "tag": "warranty"
                    }
                ],
                "permissions": [
                    {
                        "description": "This software and derivatives may be used for commercial purposes.",
                        "label": "Commercial use",
                        "tag": "commercial-use"
                    },
                    {
                        "description": "This software may be modified.",
                        "label": "Modification",
                        "tag": "modifications"
                    },
                    {
                        "description": "This software may be distributed.",
                        "label": "Distribution",
                        "tag": "distribution"
                    },
                    {
                        "description": "This software may be used and modified in private.",
                        "label": "Private use",
                        "tag": "private-use"
                    }
                ]
            },
            "spdx_id": "GPL-2.0",
            "url": "http://choosealicense.com/licenses/gpl-2.0/"
        }
    ],
    "matched_files": [
        {
            "content": "\t\t    GNU GENERAL PUBLIC LICENSE\n\t\t[...],
            "content_hash": "da72d8786703dfb3f4ccf3f9b600f948bf6d26c2",
            "content_normalized": [...]
            "filename": "COPYING",
            "matched_license": "GPL-2.0",
            "matcher": {
                "confidence": 98.7510407993339,
                "name": "dice"
            }
        },
        {
            "content": "\t\t    GNU GENERAL PUBLIC LICENSE\n\t\t[...]",
            "content_hash": "da72d8786703dfb3f4ccf3f9b600f948bf6d26c2",
            "content_normalized": [...],
            "filename": "COPYING",
            "matched_license": "GPL-2.0",
            "matcher": {
                "confidence": 98.7510407993339,
                "name": "dice"
            }
        }
    ]
}

```

## Expected convertet output
```
"COPYING";"GPL-2.0";"confidence: 98.7510407993339"
```
