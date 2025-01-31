test_that("plot_beta_distrib() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_beta_distrib()", {
  expect_snapshot({
    therms_dt <- data.table::as.data.table(RregressPkg::Therms18) |>
      _[, .(na.omit(fttrump))] |>
      data.table::setnames(old = "V1",new = "Rating")

    beta_est_lst <- RregressPkg::plot_beta_distrib(
      n = nrow(therms_dt),
      mean = mean(therms_dt$Rating),
      sd = sd(therms_dt$Rating),
      min_val = 0,
      max_val = 100,
      seed = 8675309,
      x_title = "Trump Rating",
      y_title = "Count",
      bar_fill = "blue",
      bar_alpha = 0.5,
      y_limits = c(0, 600),
      y_major_breaks = seq(from = 0, to = 600, by = 50)
    )
    a_plot <- beta_est_lst$histo_plot
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_beta_distrib()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
