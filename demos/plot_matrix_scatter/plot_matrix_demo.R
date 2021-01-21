library(ggplot2)
library(here)
library(data.table)
library(grid)
library(gtable)
library(RplotterPkg)
library(RregressPkg)

current_dir <- here()
data_path <- file.path(current_dir, "demos/data/Advertising.csv")
advertise_dt <- data.table::fread(data_path)
advertise_dt <- advertise_dt[,.(sales,TV,radio,newspaper)]

RregressPkg::plot_matrix_scatter(
  df = advertise_dt,
  title = "Advertising Predictors of Sales",
  rot_y_tic_label = T
)
