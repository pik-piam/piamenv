# restoreRenv

Restores the current renv to the state described in the given lockfile.
If multiple lockfiles are given, ask the user which one to restore.

## Usage

``` r
restoreRenv(
  lockfile = Sys.glob(c("output/*/*renv.lock", "renv/archive/*renv.lock"))
)
```

## Arguments

- lockfile:

  One or more paths to lockfiles. The default value assumes an output
  folder with run folders containing renv.lock files, and/or an
  renv/archive folder with renv.lock files.

## Value

Invisibly, the return value of renv::restore.

## Author

Pascal Sauer
