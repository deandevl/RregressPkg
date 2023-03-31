library(ggplot2)
library(here)
library(data.table)
library(magrittr)
library(grid)
library(gtable)
library(RplotterPkg)
library(RregressPkg)

# -------------------------Data trees from base R-------------------------------------
# 31 felled cherry trees where timber volume depends on their height and trunk girth(inches)
data(trees)
str(trees)

# specify some axis scaling; a NULL values means default scaling from ggplot2
var_name_scaling_lst <- list(
  Girth = seq(5, 25, 5),
  Height = seq(60, 90, 5),
  Volume = NULL
)
RregressPkg::plot_pairs(
  df = trees, 
  var_name_scaling = var_name_scaling_lst,
  title = "Cherry Trees"
)

# -----------------------Advertising Data------------------
# Read the data:
data_path <- file.path(here::here(), "demo/data/Advertising.csv")
advertise_dt <- data.table::fread(data_path) %>%
  .[,.(TV,radio,newspaper,sales)]

# using default axis scaling by ggplot2
var_name_scaling_lst <- list(
  TV = NULL,
  radio = NULL,
  newspaper = NULL,
  sales = NULL
)
# Plot scatter matrix:
RregressPkg::plot_pairs(
  df = advertise_dt,
  var_name_scaling = var_name_scaling_lst,
  title = "Advertising Predictors of Sales",
  rot_y_tic_label = T
)
