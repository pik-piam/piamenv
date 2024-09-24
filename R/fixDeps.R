#' Fix Dependencies
#'
#' Automatically install package versions as suggested by piamenv::checkDeps into an renv.
#'
#' @param ask Whether to ask before fixing dependencies.
#' @param requirementMet The output of piamenv::checkDeps.
#' @return Invisibly, the return value of renv::install.
#'
#' @author Pascal Sauer
#'
#' @export
fixDeps <- function(ask = FALSE, requirementMet = checkDeps(action = if (ask) "note" else "pass")) {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  unmetReqs <- requirementMet[requirementMet != "TRUE"]
  if (length(unmetReqs) == 0) {
    return(invisible(list()))
  }

  if (ask) {
    message("Try to fix automatically? (Y/n): ", appendLF = FALSE)
    if (!tolower(getLine()) %in% c("", "y", "yes")) {
      return(invisible(list()))
    }
  }
  unfixableReqs <- grep(":", unmetReqs, value = TRUE, invert = TRUE)

  # the following sub(...) will transform strings like
  # lucode2 == 1.2.3 is required, but 1.0.0 is installed - please install: lucode2@1.2.3
  # into just "lucode2@1.2.3" which can be passed to renv::install
  renvInstall <- sub("^.+: ([^:]+)$", "\\1", setdiff(unmetReqs, unfixableReqs))
  installedPackages <- renv::install(renvInstall, type = getOption("pkgType"), prompt = FALSE)

  if (length(unfixableReqs) > 0) {
    stop("Not all unfulfilled requirements could be fixed:\n",
         paste(unfixableReqs, collapse = "\n  "))
  }
  return(invisible(installedPackages))
}
