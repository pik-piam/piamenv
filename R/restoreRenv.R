#' restoreRenv
#'
#' Restores the current renv to the state described in the given lockfile.
#' If multiple lockfiles are given, ask the user which one to restore.
#'
#' @param lockfile One or more paths to lockfiles.
#'
#' @author Pascal Führlich
#' @export
restoreRenv <- function(lockfile) {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  if (length(lockfile) > 1) {
    message(paste0(seq_along(lockfile), ": ", lockfile, collapse = "\n"))
    message("Number of the lockfile to restore: ", appendLF = FALSE)
    lockfile <- lockfile[as.integer(getLine())]
  }

  renv::restore(lockfile = lockfile, clean = TRUE, prompt = FALSE)
}
