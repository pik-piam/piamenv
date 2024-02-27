#' Revert Development Versions of Packages
#'
#' All PIK-PIAM packages in the current renv that are development versions, i.e.
#' that have a non-zero fourth version number component (e.g. `0.4.3.9001`), are
#' reverted to the highest version lower than the development versions (e.g.
#' `0.4.3`).
#'
#' @md
#' @return Invisibly the return value of [`renv::install()`].

#' @export
revertDevelopmentVersions <- function() {
  if (is.null(renv::project()))
    stop("No renv active.  Try starting the R session in the project root.")

  x <- utils::installed.packages()[, "Version"]
  x <- x[intersect(names(x), piamPackages())]

  x <- Filter(Negate(is.null),
              lapply(x, function(v) {
                v <- unname(package_version(v))
                b <- paste(`class<-`(v, "list")[[1]][-4], collapse = ".")
                if (b < v)
                  return(b)
              }))

  if (length(x)) {
    invisible(renv::install(paste(names(x), x, sep = "@"), prompt = FALSE))
  }
}
