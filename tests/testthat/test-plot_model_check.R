test_that("plot_model_check() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("ggplotify", quietly = TRUE))
  expect_true(requireNamespace("gtable", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_model_check()", {
  expect_snapshot({
    set.seed(1234)
    sample_size = 500
    x <- runif(n = sample_size) * 5
    y <- 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = 5)
    sim_df <- data.frame(x,y)

    fit_lm <- lm(y ~ x, data = sim_df)

    sim_ols <- RregressPkg::ols_calc(
      df = sim_df,
      formula_obj = fit_lm
    )

    a_plot <- RregressPkg::plot_model_check(
      fitted_v = sim_ols$fitted_vals,
      response_v = sim_df$y,
      residual_v = sim_ols$residual_vals,
      histo_fill = "blue",
      histo_alpha = 0.5,
      ref_line_color = "darkorange"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_model_check", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
