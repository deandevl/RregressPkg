# plot_qq()

    Code
      set.seed(20200825)
      sample_obs_df <- data.frame(vals = stats::rnorm(20, 10, 3))
      a_plot <- RregressPkg::plot_qq(df = sample_obs_df, numeric_col = "vals", title = "QQ plot of Random Observations",
        subtitle = "Sample is from a normal distribution", x_title = "Theoretical Normal Quantiles",
        y_title = "Sample Quantiles", rot_y_tic_label = TRUE, ci_lines_color = "blue",
        ci_line_type = "dashed", pts_size = 4, labels_n = 3)

