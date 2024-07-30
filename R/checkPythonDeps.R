#' checkPythonDeps.R
#'
#' Verify the availability of REMIND's Python dependencies on the host system.
#'
#' @author Tonn Rüter
#' @export
#' @importFrom reticulate import
#' @importFrom stringr regex str_match

# ---------------------
# Utility Functions
# ---------------------

#' Create Python Version
#'
#' This function creates a Python version from its components.
#'
#' @param version, subversion, patchlevel, releaseCandidate Version components.
#' @return A list representing the Python version.
createPythonVersion <- function(major = "", minor = "", patch = "", releaseType = "", releaseVersion = "") {
  # Store the components in a named list
  return(list(
    major = as.integer(major),
    minor = as.integer(minor),
    patch = as.integer(patch),
    releaseType = as.character(releaseType),
    releaseVersion = as.integer(releaseVersion)
  ))
}

#' Extract Python Version
#'
#' This function extracts the Python version from a string.
#'
#' @param versionString Version string.
#' @return A list representing the Python version.
extractPythonVersion <- function(versionString) {
  # Check PIP 440 and implementation for an even more extensive version regex. This represents a resonable subset
  # of what to expect in the wild (https://www.python.org/dev/peps/pep-0440/#version-scheme)
  versionRegex <- regex("
    v?                    # Leading v
    (?<major>[0-9]+)\\.?  # Major version
    (?<minor>[0-9]+)?\\.? # Minor version
    (?<patch>[0-9]+)?     # Patch version
    [-_\\.]?              # Possible separator
    (?<rtype>alpha|a|beta|b|preview|pre|c|rc)? # Release type
    [-_\\.]?             # Possible separator
    (?<rnum>[0-9]+)?     # Release number
    ", comments = TRUE)
  # str_match a matrix with first column containing the entire match and then all individual group patterns with
  # columns named after the group names
  matches <- str_match(versionString, versionRegex)
  return(createPythonVersion(
    major = matches[, "major"],
    minor = matches[, "minor"],
    patch = matches[, "patch"],
    releaseType = matches[, "rtype"],
    releaseVersion = matches[, "rnum"]
  ))
}

#' Compare Python Versions
#'
#' This function compares two Python versions.
#'
#' @param operator Comparison operator. One of '==', '!=', '>', '>=', '<' or '<='
#' @param first First Python version string to compare against
#' @param second Second Python version string
#' @param strict Strict comparison of version components (e.g. 3.10 != 3.10.0)
#' @return TRUE operator is satisfied, FALSE otherwise
comparePythonVersions <- function(operator, first, second, strict = TRUE) {
  # Compare individual components of the version strings in decreasing order of importance
  for (component in c("major", "minor", "patch", "releaseVersion")) {
    # If strict comparison is not required and the component is NA in either version, skip the comparison
    if (!strict && (is.na(first[[component]]) || is.na(second[[component]]))) next
    # Choosing -1 for NA means missing version components are considered less than 0.
    # Hence, a version like 3.10 (= 3.10.NA) would be considered less than 3.10.0.
    v1 <- ifelse(is.na(first[[component]]), -1, first[[component]])
    v2 <- ifelse(is.na(second[[component]]), -1, second[[component]])
    # Since we're comparing in decreasing order of importance, we can continue if components are equal ..
    if (v1 == v2) next
    # .. or immediately return the result of the comparison if they are not equal
    return(switch(operator,
      ">=" = v1 >= v2,
      "==" = v1 == v2,
      "<=" = v1 <= v2,
      "!=" = v1 != v2,
      ">" = v1 > v2,
      "<" = v1 < v2,
      stop("Invalid operator '", operator, "'. Must be either '==', '!=', '>', '>=', '<' or '<='.")
    ))
  }
  # If all components are equal or NA (in non-strict mode), return the result of the equality comparison
  return(switch(operator,
    ">=" = TRUE,
    "==" = TRUE,
    "<=" = TRUE,
    FALSE
  ))
}

#' Extract Python Package and Version
#'
#' This function extracts the package name and version from a dependency string.
#'
#' @param dependency Dependency string.
#' @param style Style of the dependency string. Either "pip" or "conda".
#' @return A list containing the package name and version.
extractPythonPackageAndVersion <- function(dependency, style = "pip") {
  # Check if 'style' is either 'pip' or 'conda'
  if (!style %in% c("pip", "conda")) {
    stop("Invalid argument: 'style' should be either 'pip' or 'conda'")
  }

  # Split the dependency based on the 'style' type
  splitDependency <- strsplit(dependency, ifelse(style == "pip", "(==|>=|<=|>|<|!=)", "="))[[1]]

  # Check if a valid dependency was provided
  if ((style == "pip" && length(splitDependency) < 2) || (style == "conda" && length(splitDependency) < 3)) {
    stop("Invalid dependency")
  }

  # Extract the name, operator, version, and build
  name <- splitDependency[1]
  operator <- ifelse(style == "pip", splitDependency[2], "==")
  version <- extractPythonVersion(splitDependency[2])
  build <- ifelse(style == "conda", splitDependency[3], "")

  return(list(
    name = name,
    operator = operator,
    version = version,
    build = build
  ))
}

# ---------------------
# Main Functions
# ---------------------

#' Check Python Dependencies and Optionally Versions
#'
#' Checks if the required Python dependencies can be imported. Optionally check if versions are met.
#'
#' @param dependencies Vector of dependency strings.
#' @param action Action to take if a dependency is missing. Either "stop", "warn", "note", or "pass".
#' @param checkVersion Logical indicating whether to check for matching versions.
#' @return TRUE if all dependencies are installed, FALSE otherwise.
checkPythonDeps <- function(dependencies, action = "stop", checkVersion = FALSE) {
  stopifnot(action %in% c("stop", "warn", "note", "pass"))

  missingDependencies <- c()

  for (dep in dependencies) {
    packageInfo <- extractPythonPackageAndVersion(dep)
    packageName <- packageInfo$name
    requiredVersion <- packageInfo$version

    tryCatch(
      {
        package <- import(packageName)
        installedVersion <- package$`__version__`

        if (checkVersion && (is.null(installedVersion) || !comparePythonVersions(">=", installedVersion, requiredVersion))) {
          missingDependencies <- c(missingDependencies, dep)
        }
      },
      error = function(e) {
        missingDependencies <- c(missingDependencies, dep)
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

#' Check Python Dependencies and Versions
#'
#' Checks if the required Python dependencies are installed and also if version requirements are met.
#'
#' @param requiredPackages Vector of required packages with optional versions.
#' @param installedPackages Vector of installed packages with versions.
#' @param checkVersion Logical indicating whether to check for matching versions.
#' @return Vector of missing packages.
checkInstalledPackages <- function(requiredPackages, installedPackages, checkVersion = FALSE) {
  # Extract package names and versions
  requiredInfos <- purrr::map(requiredPackages, extractPythonPackageAndVersion)
  requiredNames <- purrr::map_chr(requiredInfos, "name")
  requiredVersions <- purrr::map_chr(requiredInfos, "version")

  installedInfos <- purrr::map(installedPackages, extractPythonPackageAndVersion)
  installedNames <- purrr::map_chr(installedInfos, "name")
  installedVersions <- purrr::map_chr(installedInfos, "version")

  # Check if packages are installed and versions match
  if (checkVersion) {
    missingPackages <- requiredPackages[!(requiredNames %in% installedNames & purrr::map2_lgl(requiredVersions, installedVersions, comparePythonVersions) == 0)]
  } else {
    missingPackages <- requiredPackages[!(requiredNames %in% installedNames)]
  }

  return(missingPackages)
}