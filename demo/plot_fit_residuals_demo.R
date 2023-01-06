library(data.table, quietly = T)
library(ggplot2, quietly = T)
library(magrittr)
library(wooldridge, quietly = T)
library(RplotterPkg, quietly = T)
library(RregressPkg, quietly = T)

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

# Plot the fitted versus residual values for Model 1:
RregressPkg::plot_fit_residuals(
  df = sim_data_1_df,
  formula_obj = fit_1_lm,
  subtitle = "Data from Model 1",
  x_title = "Fitted",
  y_title = "Residuals",
  trend_line = FALSE,
  zero_line_color = "darkorange",
  zero_line_size = 0.8
)

# We should look for two things in this plot:
#   1. The mean of the residuals should be roughly 0
#   2. At every fitted value, the spread of the residuals should be roughly the same.

# Constant variance is often called homoscedasticity.
# Conversely, non-constant variance is called heteroscedasticity.

# Perform the Breusch-Pagan Test for Model 1.
# The test considers the null and alternative hypothesis:

# H_0: Homoscedasticity. The errors have constant variance about the true model.
# H_1: Heteroscedasticity. The errors have non-constant variance about the true model.

BP_test_1_df <- RregressPkg::ols_BP_test_calc(
  df = sim_data_1_df,
  formula_obj = fit_1_lm
)
RplotterPkg::create_table(
  x = BP_test_1_df,
  source_note = "Model 1 studentized Breusch-Pagan test",
  container_width_px = 300
)

# For fit_1_lm we see a large p-value, so we do not reject 
# the null of homoscedasticity, which is what we would expect.

# -------------------Model 2----------------------------

set.seed(42)
sim_data_2_df <- sim_2()
fit_2_lm <- lm(y ~ x, data = sim_data_2_df)

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
  ggplot2::geom_abline(
    intercept = coef(fit_2_lm)[[1]],
    slope = coef(fit_2_lm)[[2]],
    color = "darkorange"
  )

# Plot the fitted versus residual values for Model 2:
RregressPkg::plot_fit_residuals(
  df = sim_data_2_df,
  formula_obj = fit_2_lm,
  subtitle = "Data from Model 2",
  x_title = "Fitted",
  y_title = "Residuals",
  trend_line = FALSE,
  zero_line_color = "darkorange",
  zero_line_size = 0.8
)

# For any fitted value, the residuals seem roughly centered at 0.
# This is good! The linearity assumption is not violated.
# However, we also see very clearly, that for larger fitted values,
# the spread of the residuals is larger. This is bad! The constant
# variance assumption is violated here.

# Perform the Breusch-Pagan Test for Model 2:
BP_test_2_df <- RregressPkg::ols_BP_test_calc(
  df = sim_data_2_df,
  formula_obj = fit_2_lm
)
RplotterPkg::create_table(
  x = BP_test_2_df,
  source_note = "Model 2 studentized Breusch-Pagan test",
  container_width_px = 300,
  decimals_lst = list(cols = 3,decimal = 8)
)
# For fit_2 we see a small p-value, so we reject the null of homoscedasticity.
# The constant variance assumption is violated.

# -------------------Model 3----------------------------
set.seed(42)
sim_data_3_df <- sim_3()
fit_3_lm <- lm(y ~ x, data = sim_data_3_df)

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
  ggplot2::geom_abline(
    intercept = coef(fit_3_lm)[[1]],
    slope = coef(fit_3_lm)[[2]],
    color = "darkorange"
  )

# Plot the fitted versus residual values for Model 3:
RregressPkg::plot_fit_residuals(
  df = sim_data_3_df,
  formula_obj = fit_3_lm,
  subtitle = "Data from Model 3",
  x_title = "Fitted",
  y_title = "Residuals",
  trend_line = FALSE,
  zero_line_color = "darkorange",
  zero_line_size = 0.8
)

# This time on the fitted versus residuals plot, for any fitted value,
# the spread of the residuals is about the same. However, they are not even
# close to centered at zero! At small and large fitted values the model is
# underestimating, while at medium fitted values, the model is overestimating.
# These are systematic errors, not random noise. So the constant variance assumption
# is met, but the linearity assumption is violated. The form of our model is simply wrong.
# We're trying to fit a line to a curve!

# Perform the Breusch-Pagan Test for Model 3:
BP_test_3_df <- RregressPkg::ols_BP_test_calc(
  df = sim_data_3_df,
  formula_obj = fit_3_lm
)

RplotterPkg::create_table(
  x = BP_test_3_df,
  source_note = "Model 3 studentized Breusch-Pagan test",
  container_width_px = 300,
  decimals_lst = list(cols = 3,decimal = 8)
)

# ...for fit_3_lm we again see a large p-value, so we do not reject
# the null of homoscedasticity, which matches our findings with a fitted
# versus residuals plot.

# -----------------------A linear model for housing prices--------------

# The following example is from the text "Introductory Econometrics" Section 8-3 page 250,
# Testing for Heteroskedasticity by Jeffrey Wooldridge.

# Define the housing price model:
# price = -21.77 + .00207 lotsize + .123 sqrft + 13.85 bdrms

# Set up the data:
data("hprice1", package = "wooldridge")
hprice1_dt <- data.table::as.data.table(hprice1) %>%
  .[,.(price, lotsize, sqrft, bdrms)]

# Plot the predictors versus the housing price:
lotsize_plot <- RplotterPkg::create_scatter_plot(
  df = hprice1_dt,
  aes_x = "lotsize",
  aes_y = "price",
  subtitle = "Housing price vs lotsize",
  x_title = "Lotsize",
  y_title = "Housing Price"
)
sqrft_plot <- RplotterPkg::create_scatter_plot(
  df = hprice1_dt,
  aes_x = "sqrft",
  aes_y = "price",
  subtitle = "Housing price vs sqrft",
  x_title = "Sqrft",
  y_title = NULL
)
bdrms_plot <- RplotterPkg::create_scatter_plot(
  df = hprice1_dt,
  aes_x = "bdrms",
  aes_y = "price",
  subtitle = "Housing price vs bdrms",
  x_title = "Bedrooms",
  y_title = NULL
)
layout <- list(
  plots = list(lotsize_plot, sqrft_plot, bdrms_plot),
  rows = c(1,1,1),
  cols = c(1,2,3)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(2.5, 2.5, 2.5),
  row_heights = c(2.5, 2.5, 2.5)
)

# Plot the fitted versus residual values for the housing price equation
housing_price_lm <- price ~ lotsize + sqrft + bdrms
RregressPkg::plot_fit_residuals(
  df = hprice1_dt,
  formula_obj = housing_price_lm,
  subtitle = "Data from housing prices",
  x_title = "Fitted",
  y_title = "Residuals",
  trend_line = FALSE,
  zero_line_color = "darkorange",
  zero_line_size = 0.8,
  label_threshold = 100,
  label_sd = 1.0
)

# The dashed lines are positioned at +-1 standard deviation.
# Those observations with residuals beyond 100 are labeled.

# Perform the Breusch-Pagan Test for housing price model:
BP_housing_df <- RregressPkg::ols_BP_test_calc(
  df = hprice1_dt,
  formula_obj = housing_price_lm
)
RplotterPkg::create_table(
  x = BP_housing_df,
  source_note = "Housing price studentized Breusch-Pagan test",
  container_width_px = 300,
  decimals_lst = list(cols = 3,decimal = 8)
)

# With a small p-value there is strong evidence to reject
# the null hypothesis that there is a constant variance.

# Take the logarithmic form of the response/predictor variables
# of the housing price model. The cited text indicates that by
# taking the logarithmic form of the response/predictor variables,
# the heteroskedasticity is reduced.

log_formula_obj <-  log(price) ~ log(lotsize) + log(sqrft) + bdrms
RregressPkg::plot_fit_residuals(
  df = hprice1_dt,
  formula_obj = log_formula_obj,
  x_title = "Fitted",
  y_title = "Residuals",
  trend_line = FALSE,
  zero_line_color = "darkorange",
  zero_line_size = 0.8,
  label_threshold = 0.4,
  label_sd = 1.0
)

# Perform the Breusch-Pagan Test for housing price log model:
BP_log_housing_df <- RregressPkg::ols_BP_test_calc(
  df = hprice1_dt,
  formula_obj = log_formula_obj
)
RplotterPkg::create_table(
  x = BP_log_housing_df,
  source_note = "Housing log price studentized Breusch-Pagan test",
  container_width_px = 300,
  decimals_lst = list(cols = 3,decimal = 8)
)

# With the large p-value we fail to reject the null hypothesis 
# of constant variance.
