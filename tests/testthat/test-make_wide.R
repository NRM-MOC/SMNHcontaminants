test_that("make_wide returns a table", {
  expect_true(is.data.frame(make_wide(ContaminantsDB, "BFRs")))
})
