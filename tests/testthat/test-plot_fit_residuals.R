test_that("plot_fit_residuals() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_fit_residuals()", {
  expect_snapshot({
    hprice1_dt <- data.table::as.data.table(wooldridge::hprice1) |>
      _[,.(price, lotsize, sqrft, bdrms)]

    housing_price_lm <- price ~ lotsize + sqrft + bdrms
    housing_price_ols <- RregressPkg::ols_calc(
      df = hprice1_dt,
      formula_obj = housing_price_lm
    )
    a_plot <- RregressPkg::plot_fit_residuals(
      fitted_v = housing_price_ols$fitted_vals,
      residual_v = housing_price_ols$residual_vals,
      subtitle = "Data from housing prices",
      x_title = "Fitted",
      y_title = "Residuals",
      trend_line = FALSE,
      zero_line_color = "darkorange",
      zero_line_width = 0.8,
      label_threshold = 100,
      label_sd = 1.0
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_fit_residuals", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
