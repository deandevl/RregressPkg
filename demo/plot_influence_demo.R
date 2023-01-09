library(data.table)
library(wooldridge)
library(magrittr)
library(here)
library(RplotterPkg)
library(RregressPkg)

# ------------------Single/Simple Linear Regression-------------------

# Source of the data: PennState Science (https://online.stat.psu.edu/stat501/lesson/11/11.4)
#    Example 11-2 Revisited

# Set up the data:
data_path <- file.path(here::here(), "demo/data/influence2.txt")
influence2_data_dt <- data.table::fread(data_path) %>%
  .[, .(x, y)] %>%
  .[, id := lapply(1:nrow(.), function(i){paste0("ob_",i)})]

# Compute and plot the internal studentized residual:
internal_studentized_lst <- RregressPkg::plot_influence(
  df = influence2_data_dt,
  formula_obj = y ~ x,
  id_col = "id",
  influence_meas = "internal",
  label_threshold = 3.00,
  title = "Internal Studentized Influence Measure",
  subtitle = "Obs 21 internal studentized residual = 3.68; Source: PennState Science",
  rot_y_tic_label = T
)
internal_studentized_lst$plot

# Observation 21 has an internal studentized residual of 3.68 and is labeled on the plot.

# Compute and plot the external studentized residual:
external_studentized_lst <- RregressPkg::plot_influence(
  df = influence2_data_dt,
  formula_obj = y ~ x,
  id_col = "id",
  influence_meas = "external",
  label_threshold = 3.00,
  title = "External Studentized Influence Measure",
  subtitle = "Obs 21 external studentized residual = 6.69; Source: PennState Science",
  rot_y_tic_label = T
)
external_studentized_lst$plot

# Observation 21 has a external studentized residual of 6.69 and is labeled on the plot.

# Compute and plot Difference in Fits (DFFITS):
dffits_lst <- RregressPkg::plot_influence(
  df = influence2_data_dt,
  formula_obj = y ~ x,
  id_col = "id",
  influence_meas = "dffits",
  label_threshold = 0.80,
  title = "Difference in Fits Influence Measure",
  subtitle = "Source: PennState Science",
  rot_y_tic_label = T
)
dffits_lst$plot

# Compute and plot Cook's Distance
cook_lst <- RregressPkg::plot_influence(
  df = influence2_data_dt,
  formula_obj = y ~ x,
  id_col = "id",
  influence_meas = "cook",
  label_threshold = 0.30,
  title = "Cook's Distance Influence Measure",
  subtitle = "Obs 21 Cook's Distance = 0.36; Source: PennState Science",
  rot_y_tic_label = T
)
cook_lst$plot

# Observation 21 has a Cook's Distance influence of 0.36 and is labeled on the plot.

#-----------------Multiple linear regression-------------------------

# Source of the data: "Introductory Econometrics" by Jeffrey Wooldridge, Section 9-5, page 297
data("rdchem",package = "wooldridge")
rdchem_dt <- data.table::as.data.table(rdchem) %>%
  .[, .(rdintens, sales, profmarg)]

# Compute and plot Cook's Distance:
formula_obj <- rdintens ~ sales + profmarg
rdchem_influence_lst <- RregressPkg::plot_influence(
  df = rdchem_dt,
  formula_obj = formula_obj,
  influence_meas = "cook",
  label_threshold = 3.0,
  title = "Cook's Distance for Data Point Influence",
  subtitle = "Source: Wooldridge::rdchem",
  rot_y_tic_label = T
)
rdchem_influence_lst$plot

# Observation 10 has a Cook's Distance influence of 3.13 and is labeled on the plot.