library(ggplot2)
library(ggrepel)
library(data.table)
library(rlang)
library(here)
library(RplotterPkg)
library(RregressPkg)

current_dir <- here::here()
data_dir <- file.path(current_dir, "demos/data")

# ------ Multiple Linear Regression Example -----------------
# IQ as the response with brain and height size as the predictors
# Source: PennState Science (https://online.stat.psu.edu/stat462)
data_path <- file.path(data_dir, "piq.txt")
piq_data_dt <- data.table::fread(data_path)
piq_brain_height_dt <- piq_data_dt[,.(PIQ, Brain, Height)]

fit_residual_piq <-  RregressPkg::plot_fit_residuals(
  data_df = piq_brain_height_dt,
  resp_str = "PIQ",
  label_threshold = 25,
  title = "IQ vs Brain and Height Size",
  subtitle = "Source: PennState Science; Label Threshold: 25",
  rot_y_tic_label = T
)
fit_residual_piq$plot

# ------------Single/Simple Linear Regression Example -----------
# plot a data set with an obvious outlier
# note that observation 21 has a external(also known as deleted) studentized residual of 6.69
#  and it appears on the plot.
# Source: PennState Science (https://online.stat.psu.edu/stat462) Example 11-2 Revisited
data_path <- file.path(data_dir, "influence2.txt")
influence2_data_dt <- data.table::fread(data_path)
influence2_x_y_dt <- influence2_data_dt[, .(x, y)]

fit_residual_influence <- RregressPkg::plot_fit_residuals(
  data_df = influence2_x_y_dt,
  resp_str = "y",
  label_threshold = 5,
  title = "Data Set With Large External Studentized Residual",
  subtitle = "Obs 21 External Studentized Resid = 6.69; Source: PennState Science",
  rot_y_tic_label = T
)
fit_residual_influence$plot

# ------ Multiple Linear Regression Example -----------------
# Sales as a response to investments in TV and radio advertising
# Model has an interaction term
# Source: "An Introduction to Statistical Learning" page 87
data_path <- file.path(data_dir, "Advertising.csv")
advertise_dt <- data.table::fread(data_path)
advertise_inter_dt <- advertise_dt[, .(sales, TV, radio, TVxradio = TV * radio)]

fit_residual_advertise <- RregressPkg::plot_fit_residuals(
  data_df = advertise_inter_dt,
  resp_str = "sales",
  label_threshold = 3,
  title = "Sales Regressed on TV, Radio, and TVxRadio",
  subtitle = "Model shows an R-square of 96.8% with interaction term",
  rot_y_tic_label = T
)
fit_residual_advertise$plot

