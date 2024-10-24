#' archivePythonEnv
#'
#' Write Python virtual environment lock file specifying package versions currently installed in the given
#' virtual environment. Using the lock file, a new virtual environment can be built containing exactly the same
#' package versions.
#'
#' @param outputDir Path to the directory where the lock file will be written
#' @param pythonConfig as produced by \code{\link{checkPythonEnv}}. If not provided, the RETICULATE_PYTHON environment 
#'   variable is used
#' @return Path to the lock file
#'
#' @author Mika Pflüger, Tonn Rüter
#' @importFrom yaml as.yaml yaml.load
#' @importFrom utils head
#' @export
archivePythonEnv <- function(outputDir, pythonConfig = reticulate::py_config()) {
  if (!dir.exists(outputDir)) {
    stop("Provided path '", outputDir, "' does not exist.")
  }
  dateAndTime <- format(Sys.time(), "%y%m%dT%H%M%S")
  # Get the path to the Python executable
  # Check if (ana)conda, venv or system Python is used
  if (any(as.logical(c(pythonConfig$conda, pythonConfig$anaconda)))) {
    envType <- "conda"
    logFile <- file.path(outputDir, paste0("lock_", dateAndTime, "_", envType, ".txt"))
    # First run the standard conda command to create an environment file. It'll be yaml formatted, which is nice because
    # we might need to manipulate it later ..
    condaEnv <- yaml.load(system2("conda", args = c("env", "export"), stdout = TRUE))
    # .. in case some packages are installed via pip, we need to replace them with the actual pip freeze output
    # because the conda env export command does not include information about packages that have been installed
    # from particular sources repositories (i.e. it would show a version string like "0.1.4a0" instead of the
    # " @ git+https://<repo-url>/<package name>.git@<commit-hash>"). However, this is important for reproducibility and
    # a open issue with conda: https://github.com/conda/conda/issues/10320
    # Let's start by checking whether there are any pip-installed packages in the first place. The Conda yaml contains
    # those in a dedicated "pip"-section. `condaEnv$dependencies` is a list of lists with the pip-installed packages
    # being the only named ones. Check for
    pipIdx <- match(TRUE, grepl("^pip", names(unlist(condaEnv$dependencies))))
    # This vector should not contain more than one element. If there'd be more than one element, there's something
    # wrong with the conda environment file. Don't fail explicitly here, just disregard other pip-installed packages
    # except the first one.
    if (!is.na(pipIdx)) {
      # Run python -m pip [...] here instead of pip [...] cause from within the conda environment, there a chance
      # that system pip is used instead of pip that comes with the conda installation
      pipDeps <- system2("python", args = c("-m", "pip", "freeze", "--local"), stdout = TRUE)
      condaEnv$dependencies[[pipIdx]]$pip <- pipDeps
    }
    # Dump conda environment + improved pip-installed packages to yaml string
    envAsString <- as.yaml(condaEnv)
  } else {
    if (pythonConfig$virtualenv != "") {
      envType <- "venv"
      # Need to use --local to avoid listing globally installed packages
      archiveArgs <- c("-m", "pip", "freeze", "--local")
    } else {
      envType <- "system"
      archiveArgs <- c("-m", "pip", "freeze")
    }
    envAsString <- system2(normalizePath(pythonConfig$python), args = archiveArgs, stdout = TRUE)
  }
  logFile <- normalizePath(file.path(outputDir, paste0("log_pyenv_", dateAndTime, "_", envType, ".txt")),
    mustWork = FALSE
  )
  writeLines(envAsString, logFile)
  return(logFile)
}
