library(ggplot2)
library(ggrepel)
library(data.table)
library(rlang)
library(here)
library(RplotterPkg)
library(RregressPkg)

current_dir <- here::here()
data_dir <- file.path(current_dir, "demos/data")

# ------------Single/Simple Linear Regression Example -----------
# plot a data set with an obvious outlier
# note that observation 21 has a external(also known as deleted) studentized residual of 6.69
#  and it appears on the plot.
# Source: PennState Science (https://online.stat.psu.edu/stat462) Example 11-2 Revisited
data_path <- file.path(data_dir, "influence2.txt")
influence2_data_dt <- data.table::fread(data_path)
influence2_x_y_dt <- influence2_data_dt[, .(x, y)]

plot_influence <- RregressPkg::plot_obs_influence(
  data_df = influence2_x_y_dt,
  resp_str = "y",
  label_threshold = 3.00,
  title = "Data Set With Large Influential Measure",
  subtitle = "Obs 21 studentized residual = 6.69; Source: PennState Science",
  rot_y_tic_label = T
)
plot_influence$plot

# ---------------------foot-height data with 1 influential point at observation 29------
# Using Cook's distance for measuring influence
data_path <- file.path(data_dir, "height_foot.txt")
height_foot_data_dt <- data.table::fread(data_path)
plot_influence <- RregressPkg::plot_obs_influence(
  data_df = height_foot_data_dt,
  resp_str = "foot",
  influence_meas = "cook",
  label_threshold = 3.0,
  title = "Cook's Distance Influential Measure",
  subtitle = "Male height vs foot",
  rot_y_tic_label = T
)
plot_influence$plot
