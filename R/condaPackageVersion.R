#' condaPackageVersion
#'
#' Query the version of a Python package installed in a conda environment, as recorded in the
#' package metadata (`importlib.metadata`). Reads the environment's own Python interpreter
#' directly, so it requires neither an activated conda nor `module load` (unlike [condaRun]).
#' Fails hard with a descriptive error if the version cannot be established.
#'
#' @param package Distribution/import name of the Python package to query, e.g. "climate_assessment"
#' @param path Path to the conda environment prefix
#' @return Trimmed, single-line version string as reported by `importlib.metadata`
#' @export
condaPackageVersion <- function(package, path) {
  python <- file.path(normalizePath(path, mustWork = TRUE), "bin", "python")
  if (!file.exists(python)) {
    stop("condaPackageVersion -- could not find python in conda env: ", python)
  }
  code <- paste0("from importlib.metadata import version; print(version('", package, "'))")
  version <- system2(python, c("-c", shQuote(code)), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(version, "status"))) {
    stop("condaPackageVersion -- querying version of '", package, "' failed: ",
         paste(version, collapse = " "))
  }
  if (length(version) != 1 || !nzchar(trimws(version))) {
    stop("condaPackageVersion -- expected a single non-empty version line for '", package,
         "', got: ", paste(deparse(version), collapse = ""))
  }
  trimws(version)
}
