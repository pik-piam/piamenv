#' checkPythonEnv.R
#'
#' Determine the host system's Python installation
#'
#' @param path Optional. Fully qualified path to the Python environment
#'
#' @author Tonn Rüter
#' @importFrom reticulate use_condaenv use_virtualenv use_python py_discover_config
#' @export
checkPythonEnv <- function(path, verbose = FALSE) {
  # Check if the directory exists
  if (!dir.exists(path)) {
    stop("Provided path '", path, "' does not exist.")
  }
  # Heuristic to guess and possibly activate the provided Python environment
  if (file.exists(file.path(path, "conda-meta"))) {
    tryCatch(
      {
        use_condaenv(path, required = TRUE)
        if (verbose) message("Using conda/Python environment '", path, "'")
      },
      error = function(e) {
        stop("Unable to use conda environment '", path, "'")
      }
    )
  } else if (file.exists(file.path(path, "pyvenv.cfg"))) {
    tryCatch(
      {
        use_virtualenv(path, required = TRUE)
        if (verbose) message("Using venv/Python environment ", path, "'")
      },
      error = function(e) {
        stop("Unable to use venv environment '", path, "'")
      }
    )
  } else if (file.exists(file.path(path, "bin/python")) || file.exists(file.path(path, "python.exe"))) {
    use_python(path)
    if (verbose) {
      message("No virtual Python environment detected. Using Python '", py_discover_config()$python, "'")
      message("WARNING: Using system Python is not recommended.")
    }
  } else {
    # path likely does not contain a Python environment, try use_python anyway, since it will raise an error
    use_python(path)
  }
  return(py_discover_config())
}
