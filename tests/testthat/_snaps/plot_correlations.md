# plot_correlations()

    Code
      midwest_dt <- data.table::as.data.table(ggplot2::midwest)[, .(popdensity,
        percwhite, percblack, popadults, perchsd, percollege, percprof,
        percbelowpoverty, percchildbelowpovert, percadultpoverty, percelderlypoverty,
        inmetro)]
      midwest_corr_lst <- RregressPkg::plot_correlations(x = midwest_dt, title = "Correlations of Midwest Demographic Variables",
        subtitle = "Source: ggplot2::midwest", label_sz = 6)
      a_plot <- midwest_corr_lst$corr_plot

