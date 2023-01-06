library(data.table)
library(ggplot2)
library(ggrepel)
library(RplotterPkg)
library(RregressPkg)

# ------------QQ plot of random normal sample------------------

# Set the data:
set.seed(20200825)
sample_obs_df <- data.frame(
  vals = stats::rnorm(20, 10, 3)
)
# The random sample of 20 values has a mean of 10 and standard deviation of 3.

# insert a large negative value
sample_obs_df$vals[[3]] <-  -17

# Plot the QQ plot:
RregressPkg::plot_qq(
  df = sample_obs_df,
  numeric_col = "vals",
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

# Re-plot with the standardized values:
RregressPkg::plot_qq(
  df = sample_obs_df,
  numeric_col = "vals",
  standardize = TRUE,
  title = "QQ plot of Random Observations",
  subtitle = "Sample is from a normal distribution",
  x_title = "Theoretical Normal Quantiles",
  y_title = "Standardized Sample Quantiles",
  rot_y_tic_label = TRUE,
  ci_lines_color = "blue",
  ci_line_type = "dashed",
  pts_size = 4,
  labels_n = 3
)