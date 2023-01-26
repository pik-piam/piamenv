#' writePythonVirtualEnvLockFile
#'
#' Write Python virtual environment lock file specifying package versions currently installed in the given
#' virtual environment. Using the lock file, a new virtual environment can be built containing exactly the same
#' package versions.
#'
#' @param filePath Path where the virtual environment lock file will be created.
#' @param venv Path to Python virtual environment folder.
#'
#' @author Mika Pflüger
#' @export
writePythonVirtualEnvLockFile <- function(filePath, venv = ".venv") {
  packageVersions <- system2(pythonBinPath(venv), c("-mpip", "freeze", "--local"), stdout = TRUE)
  writeLines(packageVersions, filePath)
  return(invisible(TRUE))
}
