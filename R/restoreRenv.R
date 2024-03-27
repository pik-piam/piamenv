#' restoreRenv
#'
#' Restores the current renv to the state described in the given lockfile.
#' If multiple lockfiles are given, ask the user which one to restore.
#'
#' @param lockfile One or more paths to lockfiles. The default value assumes an output folder with
#' run folders containing renv.lock files, and/or an renv/archive folder with renv.lock files.
#' @return Invisibly, the return value of renv::restore.
#'
#' @author Pascal Sauer
#' @export
restoreRenv <- function(lockfile = Sys.glob(c("output/*/*renv.lock", "renv/archive/*renv.lock"))) {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  if (length(lockfile) > 1) {
    message(paste0(seq_along(lockfile), ": ", lockfile, collapse = "\n"))
    message("Number of the lockfile to restore: ", appendLF = FALSE)
    lockfile <- lockfile[as.integer(getLine())]
  }

  return(invisible(renv::restore(lockfile = lockfile, clean = TRUE, prompt = FALSE)))
}
