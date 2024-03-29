#' showUpdates
#'
#' Print available updates of the given packages.
#'
#' @param packages A character vector of package names.
#' @return Invisibly, a data.frame as returned by utils::old.packages
#'
#' @export
showUpdates <- function(packages = piamPackages()) {
  installed <- utils::installed.packages()
  instPkgs <- installed[installed[, "Package"] %in% packages, , drop = FALSE]
  withr::with_options(list(timeout = 15),
    outdatedPackages <- utils::old.packages(instPkgs = instPkgs)
  )
  if (!is.null(outdatedPackages)) {
    message("The following updates are available:\n",
            paste("-", outdatedPackages[, "Package"], ":",
                  outdatedPackages[, "Installed"], "->", outdatedPackages[, "ReposVer"],
                  collapse = "\n"))
  }
  return(invisible(outdatedPackages))
}
