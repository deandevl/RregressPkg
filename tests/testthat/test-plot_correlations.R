test_that("plot_correlations() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_correlations()", {
  expect_snapshot({
    midwest_dt <- data.table::as.data.table(ggplot2::midwest)  |>
      _[, .(popdensity, percwhite, percblack, popadults, perchsd,
            percollege, percprof, percbelowpoverty, percchildbelowpovert,
            percadultpoverty, percelderlypoverty, inmetro)
       ]

    midwest_corr_lst <- RregressPkg::plot_correlations(
      x = midwest_dt,
      title = "Correlations of Midwest Demographic Variables",
      subtitle = "Source: ggplot2::midwest",
      label_sz = 6
    )
    a_plot <- midwest_corr_lst$corr_plot
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_correlations()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
