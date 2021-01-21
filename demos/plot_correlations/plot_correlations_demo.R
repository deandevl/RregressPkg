library(data.table)
library(rlang)
library(ggplot2)
library(RplotterPkg)
library(RregressPkg)

midwest_df <- ggplot2::midwest[, c(6, 12, 13, 17, 18, 19, 20, 23, 24, 25, 26, 27)]
str(midwest_df)

midwest_corr_mt <- round(stats::cor(midwest_df), 2)

# without clustering
RregressPkg::plot_correlations(
  corr = midwest_corr_mt,
  title = "Correlations Among Selected Midwest Variables",
  subtitle = "Data source: ggplot2::midwest",
  center_titles = TRUE,
  legend_title = "Degree of Correlation"
)

# with clustering
RregressPkg::plot_correlations(
  corr = midwest_corr_mt,
  cluster_corr = TRUE,
  title = "Correlations Among Selected Midwest Variables",
  subtitle = "Data source: ggplot2::midwest",
  center_titles = TRUE,
  legend_title = "Degree of Correlation"
)