#' stopIfLoaded
#'
#' Throw an error if any of the given packages is loaded.
#'
#' This is useful after updating packages. If any of the updated packages was loaded before
#' the update R might crash when trying to lazy load a function from the updated package.
#'
#' @param updatedPackage One or more names of packages that were just updated.
#'
#' @author Pascal Sauer
#' @examples
#' \dontrun{
#' updates <- piamenv::fixDeps()
#' piamenv::stopIfLoaded(names(updates))
#'
#' updates <- piamenv::updateRenv()
#' piamenv::stopIfLoaded(names(updates))
#' }
#'
#' @export
stopIfLoaded <- function(updatedPackage) {
  updatedAndLoaded <- Filter(updatedPackage, f = isNamespaceLoaded)
  if (length(updatedAndLoaded) > 0) {
    stop("The following packages were updated, but also previously loaded:\n",
         paste("-", updatedAndLoaded, collapse = "\n  "),
         "\n  Please restart R.")
  }
}
