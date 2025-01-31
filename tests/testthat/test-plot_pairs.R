test_that("plot_pairs() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("ggplotify", quietly = TRUE))
  expect_true(requireNamespace("gtable", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_pairs()", {
  expect_snapshot({
    var_name_scaling_lst <- list(
      TV = NULL,
      radio = NULL,
      newspaper = NULL,
      sales = NULL
    )

    a_plot <- RregressPkg::plot_pairs(
      df = RregressPkg::advertising,
      var_name_scaling = var_name_scaling_lst,
      title = "Advertising Predictors of Sales",
      rot_y_tic_label = TRUE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_pairs", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("plot_pairs() scaled ", {
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("ggplotify", quietly = TRUE))
  expect_true(requireNamespace("gtable", quietly = TRUE))
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
  expect_snapshot({
    data(trees)

    var_name_scaling_lst <- list(
      Girth = seq(5, 25, 5),
      Height = seq(60, 90, 5),
      Volume = NULL
    )
    a_plot <- RregressPkg::plot_pairs(
      df = trees,
      var_name_scaling = var_name_scaling_lst,
      title = "Cherry Trees"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_pairs scaled", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
