library(ggplot2)
library(rlang)
library(ggrepel)
library(RplotterPkg)
library(RregressPkg)
library(car)

set.seed(20200825)
# create a random sample of numeric values from a normal distribution with mean 10 and sd of 3
sample_obs <- stats::rnorm(20, 10, 3)

# insert a large value
sample_obs[[3]] <-  -17

# plot the QQ-plot
RregressPkg::plot_qq(
  obs = sample_obs,
  title = "QQ plot of Random Observations",
  subtitle = "Sample is from a normal distribution",
  x_title = "Theoretical Normal Quantiles",
  y_title = "Sample Quantiles",
  rot_y_tic_label = TRUE,
  ci_lines_color = "blue",
  ci_line_type = "dashed",
  pts_size = 4,
  labels_n = 3
)

# plot the QQ-plot with the observations standardized
RregressPkg::plot_qq(
  obs = sample_obs,
  standardize = TRUE,
  title = "QQ plot of Random Observations",
  subtitle = "Sample is from a normal distribution",
  x_title = "Theoretical Normal Quantiles",
  y_title = "Standardized Sample Quantiles",
  rot_y_tic_label = TRUE,
  labels_n = 3
)

# plot the QQ-plot with car::qqPlot() for comparison
plot_car <- car::qqPlot(sample_obs)
plot_car
