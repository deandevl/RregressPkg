library(data.table)
library(ggplot2)
library(magrittr)
library(RplotterPkg)
library(RregressPkg)

# Checking the assumptions of three linear models

# The following is from "Applied Statistics with R" , Chapter 13 Model Diagnostics, by David Dalpiaz.
# The book is currently designed for use with STAT 420, "Methods of Applied Statistics" at the University of Illinois Urbana-Chammmpaign.

# Define three simulated models:

# Model 1: Y = 3 + 5x + e, e - N(0,1)
# Model 2: Y = 3 + 5x + e, e - N(0,x^2)
# Model 3: Y = 3 + 5x^2 + e, e - N(0,25)

sim_1 <- function(sample_size=500){
  x <- runif(n = sample_size) * 5
  y <- 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x,y)
}

sim_2 <- function(sample_size = 500){
  x <- runif(n = sample_size) * 5
  y <- 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x,y)
}

sim_3 <- function(sample_size = 500){
  x <- runif(n = sample_size) * 5
  y <- 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x,y)
}

# -------------------Model 1----------------------------
set.seed(42)
sim_data_1_df <- sim_1()
fit_1_lm <- lm(y ~ x, data = sim_data_1_df)

sim_1_ols <- RregressPkg::ols_calc(
  df = sim_data_1_df,
  formula_obj = fit_1_lm
)
# plot model 1 and its fitted line
RplotterPkg::create_scatter_plot(
  df = sim_data_1_df,
  aes_x = "x",
  aes_y = "y",
  pts_fill = "blue",
  pts_stroke = 0,
  pts_size = 2,
  pts_line_alpha = 0.2,
  subtitle = "Data from Model 1",
  x_title = "X",
  y_title = "Y"
) +
  ggplot2::geom_abline(intercept = coef(fit_1_lm)[[1]], slope = coef(fit_1_lm)[[2]], color = "darkorange")

# check model 1
RregressPkg::plot_model_check(
  fitted_v = sim_1_ols$fitted_vals,
  response_v = sim_data_1_df$y,
  residual_v = sim_1_ols$residual_vals,
  histo_fill = "blue",
  histo_alpha = 0.5,
  ref_line_color = "darkorange"
)

# --------------------Model 2---------------------------
sim_data_2_df <- sim_2()
fit_2_lm <- lm(y ~ x, data = sim_data_2_df)

sim_2_ols <- RregressPkg::ols_calc(
  df = sim_data_2_df,
  formula_obj = fit_2_lm
)
# plot model 2 and its fitted line
RplotterPkg::create_scatter_plot(
  df = sim_data_2_df,
  aes_x = "x",
  aes_y = "y",
  pts_fill = "blue",
  pts_stroke = 0,
  pts_size = 2,
  pts_line_alpha = 0.2,
  subtitle = "Data from Model 2",
  x_title = "X",
  y_title = "Y"
) +
  ggplot2::geom_abline(intercept = coef(fit_2_lm)[[1]], slope = coef(fit_2_lm)[[2]], color = "darkorange")

# check model 2
RregressPkg::plot_model_check(
  fitted_v = sim_2_ols$fitted_vals,
  response_v = sim_data_2_df$y,
  residual_v = sim_2_ols$residual_vals,
  histo_fill = "blue",
  histo_alpha = 0.5,
  ref_line_color = "darkorange"
)

# --------------------Model 3---------------------------
sim_data_3_df <- sim_3()
fit_3_lm <- lm(y ~ x, data = sim_data_3_df)

sim_3_ols <- RregressPkg::ols_calc(
  df = sim_data_3_df,
  formula_obj = fit_3_lm
)
# plot model 2 and its fitted line
RplotterPkg::create_scatter_plot(
  df = sim_data_3_df,
  aes_x = "x",
  aes_y = "y",
  pts_fill = "blue",
  pts_stroke = 0,
  pts_size = 2,
  pts_line_alpha = 0.2,
  subtitle = "Data from Model 3",
  x_title = "X",
  y_title = "Y"
) +
  ggplot2::geom_abline(intercept = coef(fit_3_lm)[[1]], slope = coef(fit_3_lm)[[2]], color = "darkorange")

# check model 2
RregressPkg::plot_model_check(
  fitted_v = sim_3_ols$fitted_vals,
  response_v = sim_data_3_df$y,
  residual_v = sim_3_ols$residual_vals,
  histo_fill = "blue",
  histo_alpha = 0.5,
  ref_line_color = "darkorange"
)
