library(ggplot2)
library(here)
library(palmerpenguins)
library(data.table)
library(magrittr)
library(grid)
library(gtable)
library(RplotterPkg)
library(RregressPkg)

# -----------------------Advertising Data------------------
# Read the data:
data_path <- file.path(here::here(), "demo/data/Advertising.csv")
advertise_dt <- data.table::fread(data_path) %>%
  .[,.(sales,TV,radio,newspaper)]

# Plot scatter matrix:
RregressPkg::plot_matrix_scatter(
  df = advertise_dt,
  title = "Advertising Predictors of Sales",
  rot_y_tic_label = T,
  plot_dim = 6.0
)

# ------------------Penguin Data------------------------
data("penguins", package = "palmerpenguins")

penguins_dt <- data.table::as.data.table(penguins) %>% 
  .[, .(
  Species = species,
  Island = island,
  Bill_Len = bill_length_mm/10,
  Bill_Dep = bill_depth_mm/10,
  Flip_Len = flipper_length_mm/100,
  Body_Mass = body_mass_g/1000,
  Sex = sex,
  Year = as.factor(year))] %>% 
  na.omit() 

# Refactor "Island" and "Species"
penguins_dt[, `:=`(Island = as.factor(fcase(Island == "Biscoe","Bis",
                                            Island == "Dream","Dr",
                                            Island == "Torgersen", "Tor")))]

penguins_dt[,`:=`(Species = as.factor(fcase(Species == "Adelie","Ad",
                                            Species == "Chinstrap","Ch",
                                            Species == "Gentoo","Ge")))]

# Plot scatter matrix:
RregressPkg::plot_matrix_scatter(
  df = penguins_dt,
  rot_y_tic_label = T,
  plot_dim = 10
)
