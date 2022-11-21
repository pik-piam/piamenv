#' Check Dependencies
#'
#' Check if package requirements specified in the given DESCRIPTION are met.
#'
#' @md
#' @param descriptionFile Path to a DESCRIPTION file or a path that belongs to a
#'   source package project.
#' @param dependencyTypes The types of dependencies to check. Must be a
#' subset of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#' @param action Action to take on unmet dependencies:
#'   - `"stop"`:  Issue an error with the unmet dependencies.  (Default.)
#'   - `"warn"`:  Issue a warning with the unmet dependencies.
#'   - `"pass"`:  Do nothing, just return invisibly.
#'   - `"ask"`: Ask the user whether to auto-fix missing dependencies. Requires an active renv.
#' @return Invisibly, a named list of strings indicating whether each package
#'   requirement is met (`"TRUE"`) or not, in which case the reason is stated.
#'
#' @author Pascal Führlich, Michaja Pehl
#'
#' @examples
#' checkDeps(system.file("DESCRIPTION", package = "piamenv"))
#'
#' @export
checkDeps <- function(descriptionFile = ".",
                      dependencyTypes = c("Depends", "Imports", "LinkingTo"),
                      action = "stop") {
  stopifnot(all(dependencyTypes %in% c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")))
  stopifnot(action %in% c("stop", "warn", "pass", "ask"))
  allDeps <- desc::desc_get_deps(descriptionFile)
  deps <- allDeps[allDeps$type %in% dependencyTypes, ]
  requirementMet <- Map(checkRequirement, package = deps$package, version = deps$version)

  if (!all(requirementMet == "TRUE")) {
    missing <- requirementMet[requirementMet != "TRUE"]
    msg <- paste(missing, collapse = "\n  ")
    if (action == "ask" && any(grepl(": ", missing))) {
      if (!requireNamespace("renv", quietly = TRUE) || is.null(renv::project())) {
        stop(msg)
      }
      message("missing requirements:\n  ", msg, "\n",
              "Try to fix automatically? (Y/n) ", appendLF = FALSE)
      if (tolower(getLine()) %in% c("", "y", "yes")) {
        renvInstall <- sub("^.+: ([^:]+)$", "\\1", grep(": ", missing, value = TRUE))
        renv::install(renvInstall, prompt = FALSE)
        renv::snapshot(prompt = FALSE)
        # after installing, check again in new R session to avoid using already loaded packages
        return(invisible(callr::r(function(...) piamenv::checkDeps(...),
                                  list(descriptionFile, dependencyTypes, action = "stop"))))
      }
    } else if (action %in% c("stop", "ask")) {
      stop(msg)
    } else if (action == "warn") {
      warning(msg)
    }
  }

  return(invisible(requirementMet))
}

checkRequirement <- function(package, version) {
  # get installed package or R version
  if (package == "R") {
    packageVersion <- getRversion()
  } else {
    packageVersion <- tryCatch({
      utils::packageVersion(package)
    }, error = function(error) NA)
  }

  # if the package is not installed
  if (is.na(packageVersion)) {
    result <- paste0(package, " is required, but not installed - please install: ", package)
  # if we don't care about the version
  } else if (version == "*") {
    result <- "TRUE"
  # compare version requirement
  } else {
    validops <- c("<", "<=", ">", ">=", "==", "!=")

    # split version into operator and version number
    opANDv <- unlist(
      regmatches(x = version,
                 m = gregexpr(pattern = paste0("(",
                                               paste(c(validops, "[0-9\\.-]+"),
                                                     collapse = "|"),
                                               ")"),
                              text = version))
    )
    operator <- opANDv[[1]]
    requiredVersion <- package_version(opANDv[[2]])

    # compare to installed version
    if (operator %in% validops) {
      if (methods::getFunction(operator)(packageVersion, requiredVersion)) {
        result <- "TRUE"
        # warn on mismatch
      } else {
        result <- paste(package, operator, requiredVersion,
                        "is required, but", packageVersion,
                        "is installed - please ")
        if (operator %in% c(">", ">=", "!=")) {
          result <- paste0(result, "update: ", package)
        } else if (operator %in% c("<=", "==")) {
          result <- paste0(result, "install: ", package, "@", requiredVersion)
        } else if (operator == "<") {
          result <- paste0(result, "install a compatible version of ", package)
        }
      }
    # catch faulty declarations
    } else {
      result <- paste("invalid dependency declaration", package, version)
    }
  }
  return(result)
}
