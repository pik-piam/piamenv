# condaPackageVersion

Query the version of a Python package installed in a conda environment,
as recorded in the package metadata (\`importlib.metadata\`). Reads the
environment's own Python interpreter directly, so it requires neither an
activated conda nor \`module load\` (unlike \[condaRun\]). Fails hard
with a descriptive error if the version cannot be established.

## Usage

``` r
condaPackageVersion(package, path)
```

## Arguments

- package:

  Distribution/import name of the Python package to query, e.g.
  "climate_assessment"

- path:

  Path to the conda environment prefix

## Value

Trimmed, single-line version string as reported by
\`importlib.metadata\`
