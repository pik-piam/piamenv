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
  installedUpdates <- renv::update(setdiff(installedPiamPackages, exclude), prompt = FALSE)

  archiveRenv()
  return(invisible(installedUpdates))
}
