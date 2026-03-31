# updateRenv

Update all PIK-PIAM packages in the current renv, write renv.lock into
archive.

## Usage

``` r
updateRenv(exclude = NULL)
```

## Arguments

- exclude:

  vector of packages not to be updated

## Value

Invisibly, the return value of renv::update.

## Author

Pascal Sauer
