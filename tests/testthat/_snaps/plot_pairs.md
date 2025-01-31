# plot_pairs()

    Code
      var_name_scaling_lst <- list(TV = NULL, radio = NULL, newspaper = NULL, sales = NULL)
      a_plot <- RregressPkg::plot_pairs(df = RregressPkg::advertising,
      var_name_scaling = var_name_scaling_lst, title = "Advertising Predictors of Sales",
      rot_y_tic_label = TRUE)

# plot_pairs() scaled 

    Code
      data(trees)
      var_name_scaling_lst <- list(Girth = seq(5, 25, 5), Height = seq(60, 90, 5),
      Volume = NULL)
      a_plot <- RregressPkg::plot_pairs(df = trees, var_name_scaling = var_name_scaling_lst,
        title = "Cherry Trees")

