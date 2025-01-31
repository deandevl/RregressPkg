# plot_beta_distrib()

    Code
      therms_dt <- data.table::setnames(data.table::as.data.table(RregressPkg::Therms18)[
        , .(na.omit(fttrump))], old = "V1", new = "Rating")
      beta_est_lst <- RregressPkg::plot_beta_distrib(n = nrow(therms_dt), mean = mean(
        therms_dt$Rating), sd = sd(therms_dt$Rating), min_val = 0, max_val = 100,
      seed = 8675309, x_title = "Trump Rating", y_title = "Count", bar_fill = "blue",
      bar_alpha = 0.5, y_limits = c(0, 600), y_major_breaks = seq(from = 0, to = 600,
        by = 50))
      a_plot <- beta_est_lst$histo_plot

