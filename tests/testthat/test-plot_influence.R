test_that("plot_influence() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_influence()", {
  expect_snapshot({
    rdchem_dt <- data.table::as.data.table(wooldridge::rdchem) |>
      _[, .(rdintens, sales, profmarg)]

    formula_obj <- rdintens ~ sales + profmarg
    rdchem_influence_lst <- RregressPkg::plot_influence(
      df = rdchem_dt,
      formula_obj = formula_obj,
      influence_meas = "cook",
      label_threshold = 3.0,
      title = "Cook's Distance for Data Point Influence",
      subtitle = "Source: Wooldridge::rdchem",
      rot_y_tic_label = TRUE
    )
    a_plot <- rdchem_influence_lst$plot
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("plot_influence", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
