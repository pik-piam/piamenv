# Revert Development Versions of Packages

All PIK-PIAM packages in the current renv that are development versions,
i.e. that have a non-zero fourth version number component (e.g.
`0.4.3.9001`), are reverted to the highest version lower than the
development versions (e.g. `0.4.3`).

## Usage

``` r
revertDevelopmentVersions()
```

## Value

Invisibly the return value of
[`renv::install()`](https://rstudio.github.io/renv/reference/install.html).
