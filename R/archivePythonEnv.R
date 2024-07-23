#' writePythonVirtualEnvLockFile
#'
#' Write Python virtual environment lock file specifying package versions currently installed in the given
#' virtual environment. Using the lock file, a new virtual environment can be built containing exactly the same
#' package versions.
#'
#' @param outputDir Path to the directory where the lock file will be written
#' @param pythonConfig as produced by \code{\link{checkPythonEnv}}. Defaults to NULL. If not provided, the
#'  RETICULATE_PYTHON environment variable is used
#'
#' @author Mika Pflüger, Tonn Rüter
#' @export
archivePythonEnv <- function(outputDir, pythonConfig = reticulate::py_discover_config()) {
  if (is.null(pythonConfig) && Sys.getenv("RETICULATE_PYTHON") == "") {
    stop("Either provide a Python configuration or set environment variable RETICULATE_PYTHON")
  }
  if (!dir.exists(outputDir)) {
    stop("Provided path '", outputDir, "' does not exist.")
  }
  # Get the path to the Python executable
  pythonBinPath <- if (is.null(pythonConfig)) Sys.getenv("RETICULATE_PYTHON") else pythonConfig$executable
  # Check if (ana)conda, venv or system Python is used
  if any(as.logical(c(pyconf$conda, pyconf$anaconda))) {
    fileSuffix <- "conda"
    archiveCmd <- c(pythonBinPath, "-mconda", "list", "--export")
  } else {
    fileSuffix <- if (pythonConfig$virtualenv != "") "venv" else "system"
    archiveCmd <- c(pythonBinPath, "-mpip", "freeze", "--local")
  }
  system(archiveCmd, stdout = file.path(outputDir, paste0("log_pyenv_", fileSuffix, ".txt")))
  return(invisible(TRUE))
}