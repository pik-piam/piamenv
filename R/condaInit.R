#' condaInit
#'
#' Determine initialization commands to make conda available
#'
#' @param how Method to activate conda if not found (e.g., "pik-cluster")
#' @param log Path to log file or NULL to use stdout
#' @param verbose Verbosity level (0 for silent, higher for more verbose)
#' @return List containing `setup` and `teardown` commands, and environment variables (`env`).
#' @export
condaInit <- function(how = "", log = NULL, verbose = 0) {
  # Make sure log is a valid abs path if set. If not set make sure output is redirected to stdout
  log <- if (!is.null(log)) normalizePath(log, mustWork = FALSE) else stdout()
  # We need to redirect stdout and stderr to the log file. This is platform dependent
  # Note: Redirect operator gott be `&>>` not `>> ... 2&>1`. The latter won't load anaconda for some reason
  redirectAndApppend <- if (.Platform$OS.type == "unix") "&>>" else "*>>"
  #
  # Conda Section: Check if conda is available and if not, provide a way to activate it
  #
  condaBin <- normalizePath(Sys.which("conda"), mustWork = FALSE)
  # .. if it isn't, the user better be on the PIK cluster or implement their own functionality
  if (condaBin == "") {
    # Add user specific conda activation procedures in this here if-else-block
    if (how == "pik-cluster") {
      currentConda <- "anaconda/2024.10"
      setupCmd <- trimws(paste(
        "module load",
        currentConda,
        if (log != stdout()) paste(redirectAndApppend, log)
      ))
      teardownCmd <- trimws(paste("module unload", currentConda))
    } else {
      stop(paste(
        "Conda not found, no activation procedure to specified.",
        "Please install conda or adapt the condaInit function to your system.",
        paste0("(how = '", how, "')")
      ))
    }
    if (verbose > 0) {
      cat(date(), " condaInit -- Setup command: \n", setupCmd, "\n", file = log, append = TRUE, sep = "")
      cat(date(), " condaInit -- Teardown command: \n", teardownCmd, "\n", file = log, append = TRUE, sep = "")
    }
  } else {
    # Conda is available, no need to do anything
    if (verbose > 0) {
      cat(date(), " condaInit -- conda found at '", condaBin, "'\n", file = log, append = TRUE, sep = "")
    }
    setupCmd <- ""
    teardownCmd <- ""
  }
  return(list(setup = setupCmd, teardown = teardownCmd))
}