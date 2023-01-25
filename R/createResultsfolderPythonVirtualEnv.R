#' createResultsfolderPythonVirtualEnv
#'
#' Create a copy of the source virtual environment in the given results folder in the subfolder `.venv`.
#'
#' @param resultsfolder Path to the resultsfolder where the new virtual environment will be created.
#' @param sourceVenv Path to the virtual environment folder which will be copied.
#'
#' @author Mika Pflüger
#' @export
createResultsfolderPythonVirtualEnv <- function(resultsfolder, sourceVenv = ".venv") {
  # create virtual env
  newPythonVirtualEnv <- file.path(resultsfolder, ".venv")
  system2(pythonBinPath(sourceVenv), c("-mvenv", newPythonVirtualEnv))
  # install packages into it
  virtualEnvLockFile <- file.path(resultsfolder, "venv.lock")
  writePythonVirtualEnvLockFile(virtualEnvLockFile, sourceVenv)
  system2(pythonBinPath(newPythonVirtualEnv), c("-mpip", "install", "-r", virtualEnvLockFile))
  return(invisible(TRUE))
}
