# copied from gms::getLine to prevent dependency on gms
getLine <- function() {
  if (interactive()) {
    # needed for e.g. RStudio and R in jupyter
    return(readline())
  }
  return(readLines(withr::local_connection(file("stdin")), n = 1))
}
