# Results
## for `time-1.7.tar.gz`
```
./wrapper.sh ../_data/time-1.7.tar.gz_extracted
/toScan
	97%	deprecated_GPL-2.0+
	97%	GPL-2.0-only
	97%	GPL-2.0-or-later
	97%	deprecated_GPL-2.0
```
no difference in percentage for **GPL-2.0-only** and **GPL-2.0-or-later**.

# Generated Output
```json
[
	{
		"project": "/time-1.7.tar.gz/",
		"matches": [
			{
				"license": "GPL-2.0-only",
				"confidence": 0.9650685
			},
			{
				"license": "deprecated_GPL-2.0",
				"confidence": 0.9650685
			},
			{
				"license": "GPL-2.0-or-later",
				"confidence": 0.9650685
			},
			{
				"license": "deprecated_GPL-2.0+",
				"confidence": 0.9650685
			}
		]
	}
]
```

## Expected convertet output
```csv
;"GPL-2.0-or-later,GPL-2.0-only";
```
**TODO**: how to express missing file level information?

## Mapping
| from                  | to                 |
|-----------------------|--------------------|
| `deprecated_GPL-2.0`  | `GPL-2.0-only`     |
| `deprecated_GPL-2.0+` | `GPL-2.0-or-later` |
