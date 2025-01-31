test_that("plot_qq() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_qq()", {
  expect_snapshot({
    set.seed(20200825)
    sample_obs_df <- data.frame(
      vals = stats::rnorm(20, 10, 3)
    )

    a_plot <- RregressPkg::plot_qq(
      df = sample_obs_df,
      numeric_col = "vals",
      title = "QQ plot of Random Observations",
      subtitle = "Sample is from a normal distribution",
      x_title = "Theoretical Normal Quantiles",
      y_title = "Sample Quantiles",
      rot_y_tic_label = TRUE,
      ci_lines_color = "blue",
      ci_line_type = "dashed",
      pts_size = 4,
      labels_n = 3
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_qq", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
