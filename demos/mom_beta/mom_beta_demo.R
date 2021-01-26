library(RregressPkg)
library(RplotterPkg)

beta_data <- RregressPkg::mom_beta(
  n = 1000,
  mean = 40.,
  sd = 40.,
  min_val = 0,
  max_val = 100,
  digits = 3,
  seed = 1004
)

RplotterPkg::create_histogram_plot(
  df = beta_data$df_scaled,
  aes_x = "data"
)

RplotterPkg::create_density_plot(
  df = beta_data$df_scaled,
  aes_x = "data"
)
