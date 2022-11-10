#' showUpdates
#'
#' Print available updates of the given packages.
#'
#' @param packages A character vector of package names. By default the result of
#' lucode2::pikPiamPackages() if lucode2 is available, otherwise must be passed explicitly.
#' @return Invisibly, a data.frame as returned by utils::old.packages
#'
#' @export
showUpdates <- function(packages = NULL) {
  if (is.null(packages)) {
    if (!requireNamespace("lucode2", quietly = TRUE)) {
      stop("showUpdates argument 'packages' must not be NULL")
    }
    packages <- lucode2::pikPiamPackages()
  }
  installed <- utils::installed.packages()
  outdatedPackages <- utils::old.packages(instPkgs = installed[installed[, "Package"] %in% packages, ])
  if (!is.null(outdatedPackages)) {
    message("The following updates are available:\n",
            paste("-", outdatedPackages[, "Package"], ":",
                  outdatedPackages[, "Installed"], "->", outdatedPackages[, "ReposVer"],
                  collapse = "\n"))
  }
  return(invisible(outdatedPackages))
}
