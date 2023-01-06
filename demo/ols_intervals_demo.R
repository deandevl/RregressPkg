library(data.table)
library(magrittr)
library(wooldridge)
library(RplotterPkg)
library(RregressPkg)

# Data source for these demo are from "Using R for Introductory Econometrics" by Florian Heiss, Section 6.2 page 130 and
# "Introductory Econometrics" by Jeffery Wooldridge, Section 6.4, page 186.

# -----------Confidence intervals of predicted college GPA from specified independent variables-----------

# The text presents the following model explaining college GPA (`colGPA`):

# colGPA = B0 + B1 sat + B2 hsperc + B3 hsize + B4 hsize^2 + mu

# Set up the data:
data("gpa2", package = "wooldridge")
gpa2_dt <- data.table::as.data.table(gpa2) %>%
  .[, .(colgpa, sat, hsperc, hsize)]

# Define the OLS formula and set target predictor values:
formula_obj <- stats::as.formula(colgpa ~ sat + hsperc + hsize + I(hsize^2))
predictor_vals_df <- data.frame(
  sat = c(1200, 900, 1400),
  hsperc = c(30, 20, 5),
  hsize = c(5, 3, 1)
)

# Call RregressPkg::ols_intervals() using the 99% confidence level:
confidence_df <- RregressPkg::ols_intervals(
  df = gpa2_dt,
  formula_obj = formula_obj,
  predictor_vals_df = predictor_vals_df,
  confid_level = 0.99
)

# Show the confidence intervals
RplotterPkg::create_table(
  x = confidence_df,
  source_note = "OLS GPA confidence intervals for specific independent values",
  container_width_px = 400
)

# Prediction intervals of predicted college GPA from specified independent variables:
prediction_df <- RregressPkg::ols_intervals(
  df = gpa2_dt,
  formula_obj = formula_obj,
  predictor_vals_df = predictor_vals_df,
  interval = "prediction",
  confid_level = 0.95
)

RplotterPkg::create_table(
  x = prediction_df,
  source_note = "OLS GPA prediction intervals for specific independent values",
  container_width_px = 400
)
