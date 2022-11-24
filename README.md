# Package environment support for PIAM

R package **piamenv**, version **0.3.1**

[![CRAN status](https://www.r-pkg.org/badges/version/piamenv)](https://cran.r-project.org/package=piamenv)  [![R build status](https://github.com/pik-piam/piamenv/workflows/check/badge.svg)](https://github.com/pik-piam/piamenv/actions) [![codecov](https://codecov.io/gh/pik-piam/piamenv/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piamenv) [![r-universe](https://pik-piam.r-universe.dev/badges/piamenv)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Enables easier management of package environments, based on renv and Python venv.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("piamenv")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Pascal Führlich <pascal.fuehrlich@pik-potsdam.de>.

## Citation

To cite package **piamenv** in publications use:

Führlich P (2022). _piamenv: Package environment support for PIAM_. R package version 0.3.1, <https://github.com/pik-piam/piamenv>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {piamenv: Package environment support for PIAM},
  author = {Pascal Führlich},
  year = {2022},
  note = {R package version 0.3.1},
  url = {https://github.com/pik-piam/piamenv},
}
```
