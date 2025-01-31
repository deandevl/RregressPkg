test_that("plot_replace_missing() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("RplotterPkg", quietly = TRUE))
  expect_true(requireNamespace("mlbench", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("plot_replace_missing()", {
  expect_snapshot({
    data("Soybean", package = "mlbench")
    for(i in 2:ncol(Soybean)){
      Soybean[,i] <- as.numeric(Soybean[,i])
    }

    columns_of_interest <- colnames(Soybean)[2:ncol(Soybean)]
    Soybean$date[[3]] <- NA
    Soybean$date[[4]] <- 99
    Soybean$leaves[[4]] <- NA
    Soybean$leaves[[5]] <- "N/A"
    Soybean$leaves[[6]] <- "na"
    Soybean$leaves[[7]] <- NA
    Soybean$leaves[[8]] <- NaN

    missing_val_fun <- function(x){
      xx <- as.numeric(x)
      return((max(xx) - min(xx))/2)
    }

    soybean_missing_lst <- RregressPkg::plot_replace_missing(
      df = Soybean,
      variables = columns_of_interest,
      replace_fun = missing_val_fun,
      miss_values = c("N/A", "na", 99),
      title = "Count of Missing Values",
      subtitle = "mlbench::Soybean data set",
      x_title = "Variable",
      y_title = "Count of Missing Values",
      bar_lwd = 0.6,
      bar_color = "white",
      bar_labels = TRUE,
      bar_label_sz = 3,
      do_coord_flip = TRUE,
      order_bars = "asc"
    )
    missing_plot <- soybean_missing_lst$missing_plot

    soybean_replaced_lst <- RregressPkg::plot_replace_missing(
      df = soybean_missing_lst$replacement_df,
      variables = columns_of_interest,
      miss_values = c("N/A", "na", 99),
      title = "Count of Missing Values",
      subtitle = "mlbench::Soybean data set",
      x_title = "Variable",
      y_title = "Count of Missing Values",
      bar_lwd = 0.6,
      bar_color = "white",
      bar_labels = TRUE,
      bar_label_sz = 3,
      do_coord_flip = TRUE,
      order_bars = "asc",
      y_major_breaks = c(0,1,2)
    )
    replaced_plot <- soybean_replaced_lst$missing_plot
  })
  expect_true(is.ggplot(missing_plot))
  vdiffr::expect_doppelganger("plot_replace_missing missing", missing_plot)
  expect_no_error(ggplot_build(missing_plot))

  expect_true(is.ggplot(replaced_plot))
  vdiffr::expect_doppelganger("plot_replace_missing replaced", replaced_plot)
  expect_no_error(ggplot_build(replaced_plot))
})

