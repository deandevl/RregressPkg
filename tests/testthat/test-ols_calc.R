test_that("ols_calc() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
})

test_that("ols_calc()", {
  expect_snapshot({
    # "Using R for Introductory Econometrics" by Florian Heiss, Section 4.1 page 105
    gpa1_dt <- data.table::as.data.table(wooldridge::gpa1) |>
      _[, skipped := -skipped] |>
      _[, .(colGPA, hsGPA, ACT, skipped)]

    ols_ls <- RregressPkg::ols_calc(
      df = gpa1_dt,
      formula_obj = colGPA ~ hsGPA + ACT + skipped
    )
    ols_ls$coef_df
  })
})
