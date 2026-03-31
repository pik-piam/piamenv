# Check Dependencies

Check if package requirements specified in the given DESCRIPTION are
met.

## Usage

``` r
checkDeps(
  descriptionFile = ".",
  dependencyTypes = c("Depends", "Imports", "LinkingTo"),
  action = "stop"
)
```

## Arguments

- descriptionFile:

  Path to a DESCRIPTION file or a path that belongs to a source package
  project. If /DEPENDENCIES exists that will be used instead.

- dependencyTypes:

  The types of dependencies to check. Must be a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.

- action:

  Action to take on unmet dependencies:

  - `"stop"`: Issue an error with the unmet dependencies. (Default.)

  - `"warn"`: Issue a warning with the unmet dependencies.

  - `"note"`: Issue a message with the unmet dependencies.

  - `"pass"`: Do nothing, just return invisibly.

  - `"ask"`: Ask the user whether to auto-fix missing dependencies.
    Requires an active renv. Will also write renv.lock. If no active
    renv is found, stops instead.

## Value

Invisibly, a named list of strings indicating whether each package
requirement is met (`"TRUE"`) or not, in which case the reason is
stated.

## Author

Pascal Sauer, Michaja Pehl

## Examples

``` r
checkDeps(system.file("DESCRIPTION", package = "piamenv"))
```
