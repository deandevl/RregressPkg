# plot_model_check()

    Code
      set.seed(1234)
      sample_size = 500
      x <- runif(n = sample_size) * 5
      y <- 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = 5)
      sim_df <- data.frame(x, y)
      fit_lm <- lm(y ~ x, data = sim_df)
      sim_ols <- RregressPkg::ols_calc(df = sim_df, formula_obj = fit_lm)
      a_plot <- RregressPkg::plot_model_check(fitted_v = sim_ols$fitted_vals,
      response_v = sim_df$y, residual_v = sim_ols$residual_vals, histo_fill = "blue",
      histo_alpha = 0.5, ref_line_color = "darkorange")

