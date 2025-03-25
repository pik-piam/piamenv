#' condaRun
#'
#' Run a command in a specified conda environment with given environment variables.
#'
#' @param cmd Command to run in the conda environment
#' @param path Path to the conda environment
#' @param env Named vector of environment variables to set, i.e., c("VAR0" = "value0", "VAR1" = "value1")
#' @param init List containing `setup` and `teardown` commands to run before and after the command
#' @param log Path to log file or NULL to use stdout
#' @param verbose Verbosity level (0 for silent, higher for more verbose).
#' @return Invisible NULL
#' @export
condaRun <- function(cmd, path, env = c(), init = list(setup = "", teardown = ""), log = NULL, verbose = 0) {
  # Make sure log is a valid abs path if set. If not set make sure output is redirected to stdout
  log <- if (!is.null(log)) normalizePath(log, mustWork = FALSE) else stdout()
  # Verify that the path to the conda environment is valid
  path <- normalizePath(path, mustWork = TRUE)
  removeTrailingSemicolons <- function(x) {
    sub(";*\\s*$", "", x)
  }
  redirectAndApppend <- if (.Platform$OS.type == "unix") "&>>" else "*>>"
  # Embed the actual command into the conda run command, redirect stdout/stderr to the log file if set
  # Filter out empty strings, and collapse into a single command string
  condaRunCmd <- paste(
    Filter(nzchar, c(
      "conda run -p", path,
      if (verbose > 0) paste0("-", strrep("v", verbose)) else "",
      cmd,
      if (log != stdout()) paste(redirectAndApppend, log) else ""
    )),
    collapse = " "
  )
  # Then repeat with setup and teardown commands:
  # Filter out empty strings, remove trailing semicolons, and collapse into a single string
  callConda <- paste(
    Filter(nzchar, lapply(c(init$setup, condaRunCmd, init$teardown), removeTrailingSemicolons)),
    collapse = "; "
  )
  # Environment Variables Section: Just print the environment variables that are going to be set when calling
  # `withr::with_envvar`
  if (length(env) > 0 && verbose > 0) {
    cat(
      date(), " condaRun -- Environment variables:\n",
      paste0("   ", names(env), "=", env, " (sys: '", Sys.getenv(names(env)), "')", collapse = "\n"), "\n",
      file = log, append = TRUE, sep = ""
    )
  }
  # More verbose logging
  if (verbose > 0) {
    cat(paste0(date(), " condaRun -- About to run\n", callConda, "\n"), file = log, append = TRUE)
  }
  # Run the command in a new environment
  withr::with_envvar(
    env,
    {
      system(callConda)
    },
    action = "replace"
  )
  invisible(NULL)
}
