test_that("plot_pca() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("ggplotify", quietly = TRUE))
  expect_true(requireNamespace("gtable", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_pca()", {
  expect_snapshot({
    measurements <- colnames(RregressPkg::pilots)[2:7]
    pilots_pca_lst <- RregressPkg::plot_pca(
      df = RregressPkg::pilots,
      measures = measurements,
      center = TRUE,
      scale. = TRUE,
      rank. = 4,
      aes_fill = "Group",
      pts_size = 2,
      x_limits = c(-4, 2),
      x_major_breaks = seq(-4, 2, 1),
      title = "Principal Components of Pilots and Apprentices",
      subtitle = "6 tested attributes from 20 pilots and 20 apprentices"
    )
    pca <- pilots_pca_lst$pca
    pca_percent <- pilots_pca_lst$percent_var
    figure_plot <- pilots_pca_lst$figure_plot

    pca

    summary(pca)
  })
  expect_true(is.ggplot(figure_plot))
  vdiffr::expect_doppelganger("plot_pca figure plot", figure_plot)
  expect_no_error(ggplot_build(figure_plot))
})
