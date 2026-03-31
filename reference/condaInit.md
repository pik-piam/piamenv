# condaInit

Determine initialization commands to make conda available

## Usage

``` r
condaInit(how = "", log = NULL, verbose = 0)
```

## Arguments

- how:

  Method to activate conda if not found (e.g., "pik-cluster")

- log:

  Path to log file or NULL to use stdout

- verbose:

  Verbosity level (0 for silent, higher for more verbose)

## Value

List containing \`setup\` and \`teardown\` commands, and environment
variables (\`env\`).
