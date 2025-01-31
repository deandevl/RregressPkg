test_that("ols_restricted_model() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
})

test_that("ols_restricted_model()", {
  expect_snapshot({
    mlb1_dt <- data.table::as.data.table(wooldridge::mlb1) |>
      _[, .(salary, years, gamesyr, bavg, hrunsyr, rbisyr)]
    ur_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr)
    r_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr)

    restricted_salary_lst <- RregressPkg::ols_restricted_model(
      df = mlb1_dt,
      ur_formula_obj = ur_formula_obj,
      r_formula_obj = r_formula_obj,
      confid_level = 0.99
    )
    restricted_salary_lst$F_df$F.Value
    restricted_salary_lst$F_df$Critical
  })
})
