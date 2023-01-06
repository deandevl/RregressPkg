library(data.table)
library(ggplot2)
library(magrittr)
library(wooldridge)
library(RplotterPkg)
library(RregressPkg)

# -----------Effect of *rooms* on house price----------------

# The source for the data is from "Using R for Introductory Econometrics"
# by Forian Heiss, Section 6.2.3, page 133.

# Set up the data:
data("hprice2", package = "wooldridge")

hprice2_dt <- data.table::as.data.table(hprice2) %>%
  .[, .(price, nox, dist, rooms, stratio)]

# Define the OLS model:
formula_obj <- log(price) ~ log(nox) + log(dist) + rooms + I(rooms^2) + stratio

# Set five prospective values for the predictor *rooms*:
rooms_vals_df <- data.frame(rooms = c(4,5,6,7,8))

# Call `RregressPkg::plot_predictor()`:
rooms_effect_lst <- RregressPkg::plot_predictor(
  df = hprice2_dt,
  formula_obj = formula_obj,
  predictor_vals_df = rooms_vals_df,
  title = "Influence of 'rooms' on 'log(price)'",
  subtitle = "Formula: log(price) ~ log(nox) + log(dist) + rooms + rooms^2 + stratio",
  x_title = "rooms",
  y_title = "log(price)",
  rot_y_tic_label = T
)

# Plot the effect of 'rooms' on 'log(price)':
rooms_effect_lst$effect_plot

# Plot the effect for many values of 'rooms':
rooms_vals_df <- data.frame(rooms = seq(4, 8, 0.1))
rooms_effect_lst <- RregressPkg::plot_predictor(
  df = hprice2_dt,
  formula_obj = formula_obj,
  predictor_vals_df = rooms_vals_df,
  title = "Influence of 'rooms' on 'log(price)'",
  subtitle = "Formula: log(price) ~ log(nox) + log(dist) + rooms + rooms^2 + stratio",
  x_title = "rooms",
  y_title = "log(price)",
  rot_y_tic_label = T
)
rooms_effect_lst$effect_plot
