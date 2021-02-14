library(ggplot2)
library(ggrepel)
library(data.table)
library(here)
library(wooldridge)
library(RplotterPkg)
library(RregressPkg)

current_dir <- here::here()
data_dir <- file.path(current_dir, "demos/data")

# ------------Single/Simple Linear Regression Example -----------
# plot a data set of 21 observations with an obvious outlier
# Source: PennState Science (https://online.stat.psu.edu/stat501/lesson/11/11.4)
# Example 11-2 Revisited
data_path <- file.path(data_dir, "influence2.txt")
influence2_data_dt <- data.table::fread(data_path)
influence2_x_y_dt <- influence2_data_dt[, .(x, y)]
influence2_x_y_dt[, id := lapply(1:nrow(influence2_data_dt), function(i){paste0("ob_",i)})]
# observation 21 has an internal studentized residual of 3.68
#  and is labeled on the plot.
internal_studentized_lst <- RregressPkg::plot_influence(
  x = influence2_x_y_dt,
  id_col = "id",
  influence_meas = "internal",
  resp_col = "y",
  label_threshold = 3.00,
  title = "Internal Studentized Influence Measure",
  subtitle = "Obs 21 internal studentized residual = 3.68; Source: PennState Science",
  rot_y_tic_label = T
)
internal_studentized_lst$plot

# observation 21 has a external studentized residual of 6.69
#  and is labeled on the plot.
external_studentized_lst <- RregressPkg::plot_influence(
  x = influence2_x_y_dt,
  id_col = "id",
  influence_meas = "external",
  resp_col = "y",
  label_threshold = 3.00,
  title = "External Studentized Influence Measure",
  subtitle = "Obs 21 external studentized residual = 6.69; Source: PennState Science",
  rot_y_tic_label = T
)
external_studentized_lst$plot

# observation 21 has a Cook's Distance influence of 0.36
#  and is labeled on the plot.
cook_lst <- RregressPkg::plot_influence(
  x = influence2_x_y_dt,
  id_col = "id",
  influence_meas = "cook",
  resp_col = "y",
  label_threshold = 0.30,
  title = "Cook's Distance Influence Measure",
  subtitle = "Obs 21 Cook's Distance = 0.36; Source: PennState Science",
  rot_y_tic_label = T
)
cook_lst$plot

# observation 21 has a Difference in Fits influence of 1.55
#  and is labeled on the plot.
diffts_lst <- RregressPkg::plot_influence(
  x = influence2_x_y_dt,
  id_col = "id",
  influence_meas = "dffits",
  resp_col = "y",
  label_threshold = 1.00,
  title = "Difference in Fits Influence Measure",
  subtitle = "Obs 21 Difference in Fits = 1.55; Source: PennState Science",
  rot_y_tic_label = T
)
diffts_lst$plot

# Many observations--using row numbers for observation id's
# College grade point average with predictors on SAT scores, high school percentile,
#  high school size, and high school size squared.
# 4137 observations.
# Use Cook's Distance to identify any data points with high predictor/response values.
# A value greater than 0.50 is worthy of further investigation
# This data set does not appear to show any unusual data points
gpa_2_dt <- data.table::setDT(wooldridge::gpa2)
gpa_2_dt <- gpa_2_dt[, .(colgpa, sat, hsperc, hsize)]
gpa_2_dt[, hsize_sq := hsize^2]

colgpa_influence_lst <- RregressPkg::plot_influence(
  x = gpa_2_dt,
  resp_col = "colgpa",
  influence_meas = "cook",
  label_threshold = 0.010,
  title = "Cook's Distance for Data Point Influence",
  subtitle = "Source: Wooldridge::gpa2",
  rot_y_tic_label = T
)
# find the maximum Cook influence value
max_influence <- max(colgpa_influence_lst$influence$influence_vals)

# plot the influence values
colgpa_influence_lst$plot


