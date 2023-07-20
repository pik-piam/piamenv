# copied from lucode2 to prevent dependency
piamPackages <- function() {
  packagesUrl <- "http://pik-piam.r-universe.dev/src/contrib/PACKAGES"
  return(tryCatch({
    sort(sub("^Package: ", "", grep("^Package: ", readLines(packagesUrl), value = TRUE)))
  }, warning = function(w) piamPackagesStatic))
}

# this runs only when building the package
tryCatch({
  piamPackagesStatic <- piamPackages()
}, error = function(e) stop("http://pik-piam.r-universe.dev/src/contrib/PACKAGES not reachable"))
