# stopIfLoaded

Throw an error if any of the given packages is loaded.

## Usage

``` r
stopIfLoaded(updatedPackage)
```

## Arguments

- updatedPackage:

  One or more names of packages that were just updated.

## Details

This is useful after updating packages. If any of the updated packages
was loaded before the update R might crash when trying to lazy load a
function from the updated package.

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
updates <- piamenv::fixDeps()
piamenv::stopIfLoaded(names(updates))

updates <- piamenv::updateRenv()
piamenv::stopIfLoaded(names(updates))
} # }
```
