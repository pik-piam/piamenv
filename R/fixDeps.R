#' Fix Dependencies
#'
#' Automatically install package versions as suggested by piamenv::checkDeps into an renv.
#'
#' @param requirementMet The output of piamenv::checkDeps.
#' @return Invisibly, the return value of renv::install.
#'
#' @author Pascal Führlich
#'
#' @export
fixDeps <- function(requirementMet = checkDeps(action = "pass")) {
  stopifnot(`No renv active. Try starting the R session in the project root.` = !is.null(renv::project()))

  # sub will transform strings like
  # lucode2 == 1.2.3 is required, but 1.0.0 is installed - please install: lucode2@1.2.3
  # into just "lucode2@1.2.3" which can be passed to renv::install
  unmetReqs <- requirementMet[requirementMet != "TRUE"]
  unfixableReqs <- grep(":", unmetReqs, value = TRUE, invert = TRUE)
  renvInstall <- sub("^.+: ([^:]+)$", "\\1", setdiff(unmetReqs, unfixableReqs))
  installedPackages <- renv::install(renvInstall, prompt = FALSE)

  if (length(unfixableReqs) > 0) {
    stop("Not all unfulfilled requirements could be fixed:\n",
         paste(unfixableReqs, collapse = "\n  "))
  }
  return(invisible(installedPackages))
}
