#' updatePythonVirtualEnv
#'
#' Installs newly added requirements into the Python virtual environment.
#'
#' @param venv Path to Python virtual environment folder.
#' @param requirements Path to a requirements.txt file containing the current requirements.
#'
#' @author Mika Pflüger
#' @export
updatePythonVirtualEnv <- function(venv = ".venv", requirements = "requirements.txt") {
  system2(pythonBinPath(venv), c("-mpip", "install", "-r", requirements))
  return(invisible(TRUE))
}
