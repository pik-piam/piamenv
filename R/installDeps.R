#' installDeps
#'
#' Install dependencies, identified by renv::dependencies for a given path, into the current renv.
#'
#' @param path The path to scan for dependencies.
#'
#' @author Pascal Sauer
#'
#' @export
installDeps <- function(path) {
  deps <- renv::dependencies(path)
  installedPackages <- utils::installed.packages()[, "Package"]
  missingDeps <- setdiff(unique(deps$Package), installedPackages)
  if (length(missingDeps) > 0) {
    message("Installing missing dependencies ", paste(missingDeps, collapse = ", "))
    renv::install(missingDeps)
  }
}
