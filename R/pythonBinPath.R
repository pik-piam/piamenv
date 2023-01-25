#' pythonBinPath
#'
#' Returns the proper path to the Python binary in a given virtual environment.
#'
#' @param venv Path to Python virtual environment folder.
#' @return The full path to the Python binary in the virtual environment folder.
#'
#' @author Mika Pflüger
#' @export
pythonBinPath <- function(venv) {
  binName <- "bin/python"
  if (.Platform$OS.type == "windows") {
    # Windows needs special attention
    binName <- "Scripts/python.exe"
  }
  return(file.path(normalizePath(venv, mustWork = TRUE), binName))
}
