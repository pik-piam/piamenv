# Fix Dependencies

Automatically install package versions as suggested by
piamenv::checkDeps into an renv.

## Usage

``` r
fixDeps(
  ask = FALSE,
  requirementMet = checkDeps(action = if (ask) "note" else "pass")
)
```

## Arguments

- ask:

  Whether to ask before fixing dependencies.

- requirementMet:

  The output of piamenv::checkDeps.

## Value

Invisibly, the return value of renv::install.

## Author

Pascal Sauer
