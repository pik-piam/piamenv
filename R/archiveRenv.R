#' archiveRenv
#'
#' Writes a new <timestamp>_renv.lock based on the current package environment into renv::project()/renv/archive.
#'
#' @return Invisibly, the absolute path to the created lockfile.
#'
#' @author Pascal Sauer
#' @export
archiveRenv <- function() {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  datetime <- format(Sys.time(), "%Y-%m-%dT%H%M%S")
  archivePath <- file.path(normalizePath(renv::project()), "renv", "archive", paste0(datetime, "_renv.lock"))

  dir.create(dirname(archivePath), recursive = TRUE, showWarnings = FALSE)

  renv::snapshot(lockfile = archivePath, prompt = FALSE)

  message("Lockfile written to ", archivePath)
  return(invisible(archivePath))
}
