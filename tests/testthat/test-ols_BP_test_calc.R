test_that("ols_BP_test_calc() namespaces", {
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
})

test_that("ols_BP_test_calc()", {
  expect_snapshot({
    price_formula_obj = stats::as.formula("price~lotsize+sqrft+bdrms")
    RregressPkg::ols_BP_test_calc(
      df = wooldridge::hprice1,
      formula_obj = price_formula_obj
    )
  })
})
