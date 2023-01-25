#' writePythonVirtualEnvLockFile
#'
#' Write Python virtual environment lock file specifying package versions currently installed in the given
#' virtual environment. Using the lock file, a new virtual environment can be built containing exactly the same
#' package versions.
#'
#' @param venv Path to Python virtual environment folder.
#' @param filePath Path where the virtual environment lock file will be created.
#'
#' @author Mika Pflüger
#' @export
writePythonVirtualEnvLockFile <- function(venv = ".venv", filePath) {
  packageVersions <- system2(pythonBinPath(venv), c("-mpip", "freeze", "--local"), stdout = TRUE)
  writeLines(packageVersions, filePath)
  return(invisible(TRUE))
}
