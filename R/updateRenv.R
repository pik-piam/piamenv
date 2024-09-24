#' updateRenv
#'
#' Update all PIK-PIAM packages in the current renv, write renv.lock into archive.
#'
#' @return Invisibly, the return value of renv::update.
#' @param exclude vector of packages not to be updated
#' @author Pascal Sauer
#' @export
updateRenv <- function(exclude = NULL) {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  installedPiamPackages <- intersect(utils::installed.packages()[, "Package"], piamPackages())
  if (utils::packageVersion("renv") >= "1.0.8") {
    installedUpdates <- renv::update(setdiff(installedPiamPackages, exclude),
                                     type = getOption("pkgType"),
                                     prompt = FALSE)
  } else {
    # before renv 1.0.8 renv::update did not have the type argument
    installedUpdates <- renv::update(setdiff(installedPiamPackages, exclude), prompt = FALSE)
  }

  archiveRenv()
  return(invisible(installedUpdates))
}
