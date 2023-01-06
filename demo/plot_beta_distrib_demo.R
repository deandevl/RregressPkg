library(data.table)
library(here)
library(data.table)
library(ggplot2)
library(RregressPkg)
library(RplotterPkg)

#  Read in the 2018 ANES pilot study for Donald Trump:
data_path <- file.path(here(), "demo/data/therms18.rda")
load(data_path)

therms_dt <- data.table(
  Rating = na.omit(Therms18$fttrump)
)

# Plot the histogram of the Trump ratings:
trump_rating_plot <- RplotterPkg::create_histogram_plot(
  df = therms_dt,
  aes_x = "Rating",
  subtitle = "The Thermometer Ratings for Donald Trump",
  y_title = "Count",
  bar_fill = "blue",
  bar_alpha = 0.5,
  rot_y_tic_label = T,
  bins = 100,
  y_limits = c(0, 600),
  y_major_breaks = seq(from = 0, to = 600, by = 50)
)

beta_est_lst <- RregressPkg::plot_beta_distrib(
  n = 2500,
  mean = mean(therms_dt$Rating),
  sd = sd(therms_dt$Rating),
  min_val = 0,
  max_val = 100,
  seed = 8675309,
  subtitle = "Simulated Trump Thermometer Ratings",
  x_title = "Rating",
  y_title = "Count",
  bar_fill = "blue",
  bar_alpha = 0.5,
  y_limits = c(0, 600),
  y_major_breaks = seq(from = 0, to = 600, by = 50)
)

layout = list(
  plots = list(trump_rating_plot,beta_est_lst$histo_plot),
  rows = c(1, 2),
  cols = c(1, 1)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = 6,
  row_heights = c(3.0, 3.0),
  subtitle = "Trump Ratings With Beta Distribution Simulation"
)

