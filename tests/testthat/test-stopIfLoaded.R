test_that("stopIfLoaded works", {
  unloadNamespace("covr")
  expect_no_error({
    stopIfLoaded(c("nonexistant", "covr"))
  })
  requireNamespace("covr", quietly = TRUE)
  expectedError <- paste0("The following packages were updated, but also previously loaded:\n",
                          "- covr\n",
                          "  Please restart R.")
  expect_error({
    stopIfLoaded(c("nonexistant", "covr"))
  }, expectedError, fixed = TRUE)
})
