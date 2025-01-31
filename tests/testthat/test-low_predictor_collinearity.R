test_that("low_predictor_collinearity() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
})

test_that("low_predictor_collinearity()", {
  expect_snapshot({
    bloodpress_predictors_dt <- RregressPkg::bloodpress[, !c("Pt", "BP")]
    RregressPkg::low_predictor_collinearity(
      df = bloodpress_predictors_dt
    )
  })
})
