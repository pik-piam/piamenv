#' checkPythonDependencies.R
#'
#' Verify the availability of REMIND's Python dependencies on the host system.
#'
#' @param dependencies Character vector of Python package names to check for.
#' @param fail Logical indicating whether to stop execution if a dependency is missing. The default is TRUE.
#'
#' @author Tonn Rüter
#' @export
#' @importFrom reticulate import
checkPythonDependencies <- function(dependencies, fail = TRUE) {
  missingDependencies <- c()
  for (dep in dependencies) {
    tryCatch(
      import(dep),
      error = function(e) {
        missingDependencies <<- c(missingDependencies, dep)
      }
    )
  }
  if (length(missingDependencies) > 0 && fail) {
    stop("Missing Python dependencies: ", paste(missingDependencies, collapse = ", "))
  } else if (length(missingDependencies) > 0) {
    message("Missing Python dependencies: ", paste(missingDependencies, collapse = ", "))
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}