#' updateRenv
#'
#' Update all PIK-PIAM packages in the current renv, write renv.lock into archive.
#'
#' @author Pascal Führlich
#' @export
updateRenv <- function() {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  # update pik-piam packages only
  renv::update(intersect(utils::installed.packages()[, "Package"], piamPackages()), prompt = FALSE)

  archiveRenv()
}
