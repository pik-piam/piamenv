#' checkPythonDeps.R
#'
#' Verify the availability of REMIND's Python dependencies on the host system. Exposes two functions:
#' \code{checkPythonRequirements} and \code{checkPythonDeps}. While \code{checkPythonRequirements} reads a pip-style
#' requirements file and compares the required packages with the installed ones, \code{checkPythonDeps} checks if the
#' required Python dependencies can actually be imported in the Python environment provided.
#'
#' @author Tonn Rüter

# ---------------------
# Utility Functions
# ---------------------

#' Read Python Requirements
#'
#' This function reads a pip-style requirements file and returns a sorted vector of dependencies.
#'
#' @param path Path to the requirements file
#' @return A sorted vector of dependencies
readPythonRequirements <- function(path) {
  # Check if the file exists
  if (!file.exists(path)) {
    stop(paste0("File not found: '", path, "'"))
  }
  # Read the file line by line and remove leading and trailing whitespace
  lines <- trimws(readLines(path))
  # Remove empty lines and lines starting with, sort dependencies alphabetically
  return(sort(lines[nzchar(lines) & !startsWith(lines, "#")]))
}

#' Create Python Version
#'
#' This function creates a named list representing a Python version string from its components.
#'
#' @param major Major version number
#' @param minor Minor version number
#' @param patch Patch version number
#' @param releaseType Release type (e.g. alpha, beta, rc)
#' @param releaseVersion Release version number
#' @return A named list representing a Python version string
#' @importFrom reticulate import py_list_packages
#' @importFrom stringr regex str_match
#' @importFrom purrr map map_chr pmap
#' @importFrom utils strcapture
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

#' Print Python version string
#'
#' This function creates a string representation of a Python version.
#'
#' @param pythonVerion A named list representing the Python version.
#' @return A string representation of the Python version.
printPythonVersion <- function(pythonVerion) {
  versionString <- ""
  if (!is.na(pythonVerion$major)) {
     versionString <- paste0(versionString, as.character(pythonVerion$major))
  }
  if (!is.na(pythonVerion$minor)) {
    versionString <- paste0(versionString, ".", as.character(pythonVerion$minor))
  }
  if (!is.na(pythonVerion$patch)) {
    versionString <- paste0(versionString, ".", as.character(pythonVerion$patch))
  }
  if (!is.na(pythonVerion$releaseType)) {
    versionString <- paste0(versionString, pythonVerion$releaseType)
  }
  if (!is.na(pythonVerion$releaseVersion)) {
    versionString <- paste0(versionString, pythonVerion$releaseVersion)
  }
  return(versionString)
}

#' Extract Python Version
#'
#' This function extracts a named list representing Python version from a string by matching a regular expression.
#'
#' @param versionString A Python version string like "3.10.0a0"
#' @return A named list representing the Python version
extractPythonVersion <- function(versionString) {
  # Check PIP 440 and implementation for an even more extensive version regex. This represents a resonable subset
  # of what to expect in the wild (https://www.python.org/dev/peps/pep-0440/#version-scheme), but might need to be
  # extended for specific use cases such as post-releases or local versions.
  versionRegex <- regex("
    v?                    # Leading v
    (?<major>[0-9]+)\\.?  # Major version
    (?<minor>[0-9]+)?\\.? # Minor version
    (?<patch>[0-9]+)?     # Patch version
    [-_\\.]?              # Possible separator
    (?<rtype>alpha|a|beta|b|preview|pre|c|rc|post)? # Release type
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
#' This function compares two Python versions
#'
#' @param operator Comparison operator. One of '==', '!=', '>', '>=', '<' or '<='
#' @param first First Python version (represented as named list)
#' @param second Second Python version (represented as named list) to compare against
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

#' Create a named list representation of a Python dependency
#'
#' This function creates a named list representing a dependency on a particular Python package
#'
#' @param name Name of the dependency
#' @param operator Comparison operator. One of '==', '!=', '>', '>=', '<' or '<='
#' @param version Named list representation of the Python version of the dependency
#' @param build Build version of the dependency
#' @param repo Repository URL of the dependency
createPythonDependency <- function(name = "", operator = "", version = createPythonVersion(), build = "", repo = "") {
  return(list(
    name = name,
    operator = operator,
    version = version,
    build = build,
    repo = repo
  ))
}

#' Python dependency string with repository information
#'
#' This function creates a string representation of a Python dependency with repository information
#'
#' @param dependency Named list representation of the dependency
repoString <- function(dependency) {
  repoInfo <- c(dependency$repo, dependency$build)
  return(paste(repoInfo[repoInfo != ""], collapse = "@"))
}

#' Extract Python dependency from dependency string
#'
#' This function extracts the package name, relationship operator and version from a typical Python dependency string
#' (e.g. "numpy==1.26.4") and returns a named list representing the dependency. Dependencies on specific repositories
#' are also supported (e.g. "climate-assessment @ <URL to git repo>")
#'
#' @param depString Dependency string such as "numpy<=2.0"
#' @param style Style of the dependency string. Either "pip" or "conda"
#' @return A named list representation of the Pzthon package name and version
extractPythonDependency <- function(depString, style = "pip") {
  if (grepl(" @ ", depString)) {
    if (style == "conda") {
      warning("Invalid dependency string: Conda dependencies cannot contain repository URLs")
    }
    # Example: "climate-assessment @ <URL to git repo> -> Split at "@", first part is dependency name,
    # second part is repo URL
    pattern <- "(.*)( @ )(.*)"
    splitDepString <- strcapture(pattern, depString, proto = list(name = "", operator = "", repo = ""))
    splitRepoString <- strsplit(splitDepString$repo, "@")[[1]]
    return(createPythonDependency(
      name     = splitDepString$name,
      operator = " @ ",
      version  = createPythonVersion(),
      build    = if (!is.na(splitRepoString[2])) splitRepoString[2] else "",
      repo     = splitRepoString[1]
    ))
  } else if (style == "conda") {
    # Example: "pip=24.0=pyhd8ed1ab_0" -> Split at "=", first part is dependency name, second part is version,
    # third part is build
    splitDepString <- strsplit(depString, "=")[[1]]
    extractPythonVersion(splitDepString[2])
    return(createPythonDependency(
      name     = splitDepString[1],
      operator = "==",
      version  = extractPythonVersion(splitDepString[2]),
      build    = splitDepString[3],
      repo     = ""
    ))
  } else if (style == "pip") {
    # Example: "climate_assessment==0.1.4a0" -> Split at "==", first part is dependency name, second part is version
    pattern <- "(.*)(==|>=|<=|>|<|!=)(.*)"
    splitDepString <- strcapture(pattern, depString, proto = list(name = "", operator = "", version = ""))
    return(createPythonDependency(
      name     = splitDepString$name,
      operator = splitDepString$operator,
      version  = extractPythonVersion(splitDepString$version),
      build    = "",
      repo     = ""
    ))
  } else {
    stop("Invalid argument: 'style' should be either 'pip' or 'conda'")
  }
}

# ---------------------
# Main Functions
# ---------------------

#' Check Python Dependencies and Optionally Their Versions
#'
#' Checks if the required Python dependencies can actually be imported in the Python environment provided. Relies on
#' reticulate::import. Optionally check if versions are met
#'
#' @param dependencies Vector of dependency strings, e.g. c("numpy==1.26.4", "climate-assessment @ <URL to git repo>")
#' @param action Action to take if a dependency is missing. Either "stop", "warn", "note", or "pass"
#' @param strict Logical indicating whether to check for matching versions
#' @return TRUE if all dependencies are installed, FALSE otherwise
#' @examples
#' \dontrun{
#' deps <- c("climate_assessment==0.1.4a0", "numpy<2.0")
#' checkPythonDeps(deps, action="stop", strict = TRUE)
#' }
#' @export
checkPythonDeps <- function(dependencies, action = "stop", strict = TRUE) {
  # Keep track of missing dependencies
  faultyDependencies <- NULL
  for (dependencyString in dependencies) {
    dependency <- extractPythonDependency(dependencyString)
    # Note: Modules installed from a particular repository/fork/commit carry a version, however this version is
    # meaningless in acertaining if the correct built of the module is installed. Hence, we skip the version check in
    # this case even in strict mode and instead issue a warning
    if (strict && dependency$repo != "") {
      warning(paste0(
        "Cannot verify version of '", dependency$name, "': Make sure module was installed from ", repoString(dependency)
      ))
    }
    # Import Python packages one by one and keep track of missing dependencies or incorrect versions by adding them
    # to the faultyDependencies vector
    faultyDependencies <- c(faultyDependencies, tryCatch(
      {
        # Be adivsed: The tryCatch block heavily relies on implicit return values. SO post for more information:
        # https://stackoverflow.com/a/12195574/4453999
        # Basically, the last line of the expression provided to the tryCatch statement gets returned. To highlight this
        # *feature* of R, the following else-statements explicitly state NULL values. Note: Using return() here results
        # in returning from checkPythonDeps alltogether. That was definetly a choice ...
        pythonModule <- reticulate::import(dependency$name)
        if (strict && dependency$repo == "") {
          installedVersion <- extractPythonVersion(pythonModule$`__version__`)
          correctVersion <- comparePythonVersions(dependency$operator, installedVersion, dependency$version)
          if (!correctVersion) {
            # Make sure last statement in the expression is a single item vector containing the error message with the
            # missmatching version
            c(paste0(dependencyString, " (found version: ", printPythonVersion(installedVersion), ")"))
          } else {
            # Make sure last statement in the expression is NULL so nothing is added to the faultyDependencies vector
            NULL
          }
        } else {
          # As above
          NULL
        }
      },
      error = function(e) {
        # As above, but this time the error message contains the missing dependency
        c(paste(dependencyString, "(missing)"))
      }
    ))
  }
  # Error case: faultyDependencies handling when dependencies are missing/don't match the required version
  if (length(faultyDependencies) > 0) {
    errorMsg <- paste0("Python environment contains faulty dependencies: ", paste(faultyDependencies, collapse = ", "))
    switch(action,
      "stop" = stop(errorMsg),
      "warn" = warning(errorMsg),
      "note" = message(errorMsg),
      "pass" = {},
      stop("Invalid action '", action, "'. Must be one of 'stop', 'warn', 'note' or 'pass'.")
    )
    return(invisible(FALSE))
  }
  return(invisible(TRUE))
}

#' Check Python Dependencies and Versions
#'
#' Compares requirements given in a pip-style file with the installed Python packages. Optionally check if versions are
#' met.
#'
#' @param requirementsFile Path to a file containing the Python requirements. pip-style requirements are expected
#' @param installed Python packages installed in the environment. Defaults to reticulate::py_list_packages()
#' @param action Action if a dependency or version is missing or mismatched. Either "stop", "warn", "note", or "pass"
#' @param strict Logical indicating whether to check for strict version matching. Default is TRUE
#' @return Vector of missing packages.
#' @export
checkPythonRequirements <- function(requirementsFile, installed = py_list_packages(), action = "stop", strict = TRUE) {
  # Sanity checks for file
  if (!file.exists(requirementsFile)) {
    stop("Requirements file '", requirementsFile, "' does not exist.")
  }
  requirements <- readPythonRequirements(requirementsFile)
  requiredDependencies <- map(requirements, extractPythonDependency, style = "pip")
  requiredPackages <- map_chr(requiredDependencies, "name")
  # Now check if the required packages are installed ...
  missing <- !(requiredPackages %in% installed$package)
  present <- installed$package %in% requiredPackages
  # ... and in strict mode if they have the correct versions
  if (strict && all(!missing)) {
    # Use purrr's list comprehension to see if the installed package versions agree with the requirements. pmap applies
    # the comparison function to the list of lists element-wise. The result is a logical vector, one for each package,
    # indicating whether the version fullfills the requirement
    nomatch <- unlist(pmap(
      # We need to consider three lists: The comparison operators, the installed versions and the required versions
      list(
        map(requiredDependencies, "operator"), # Extract comparison operators from depencies
        # From all dependencies that are present in the Python environment, extract the version strings and convert them
        map(installed$version[present], extractPythonVersion),
        map(requiredDependencies, "version") # Extract required versions from deps
      ),
      comparePythonVersions
    ))
  } else {
    # If we're not in strict mode, skip the version comparison and assume all versions are correct
    nomatch <- rep(TRUE, length(requiredPackages))
  }
  # Error handling when dependencies are missing or versions mismatch
  errorMsg <- character()
  if (any(missing)) {
    errorMsg <- paste0(errorMsg, "Missing dependencies: ", paste(requiredPackages[missing], collapse = ", "), "\n")
  }
  if (any(!nomatch)) {
    errorMsg <- paste0(errorMsg, "Version mismatch: ", paste(requirements[!nomatch], collapse = ", "), "\n")
  }
  if (length(errorMsg) > 0) {
    switch(action,
      "stop" = stop(errorMsg),
      "warn" = warning(errorMsg),
      "note" = message(errorMsg),
      "pass" = return(invisible(FALSE)),
      stop("Invalid action '", action, "'. Must be one of 'stop', 'warn', 'note' or 'pass'.")
    )
  }
  return(invisible(TRUE))
}