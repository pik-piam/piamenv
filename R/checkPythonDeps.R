#' checkPythonDeps.R
#'
#' Verify the availability of REMIND's Python dependencies on the host system.
#'
#' @param dependencies Character vector of Python package names to check for.
#' @param fail Logical indicating whether to stop execution if a dependency is missing. The default is TRUE.
#'
#' @author Tonn Rüter
#' @export
#' @importFrom reticulate import
checkPythonDeps <- function(dependencies, action = "stop") {
  stopifnot(action %in% c("stop", "warn", "note", "pass"))
  missingDependencies <- c()
  for (dep in dependencies) {
    tryCatch(
      import(dep),
      error = function(e) {
        missingDependencies <<- c(missingDependencies, dep)
      }
    )
  }
  if (length(missingDependencies) > 0 && action == "stop") {
    stop("Missing Python dependencies: ", paste(missingDependencies, collapse = ", "))
  } else if (length(missingDependencies) > 0 && action == "warn") {
    warning("Missing Python dependencies: ", paste(missingDependencies, collapse = ", "))
    return(invisible(FALSE))
  } else if (length(missingDependencies) > 0 && action == "note") {
    message("Missing Python dependencies: ", paste(missingDependencies, collapse = ", "))
    return(invisible(FALSE))
  } else {
    return(invisible(TRUE))
  }
}