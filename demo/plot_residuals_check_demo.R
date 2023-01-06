library(data.table)
library(gamair)
library(magrittr)
library(RplotterPkg)
library(RregressPkg)

# --------------An example from "Generalized Additive Models"---------

# The following example is from the text "Generalized Additive Models",
# Section 1.5, page 25  by Simon Wood.

# Set up the data:
data("sperm.comp1", package = "gamair")
sperm1_dt <- data.table::as.data.table(sperm.comp1) %>%
  data.table::setnames(old = c("time.ipc","prop.partner"), new = c("time_ipc","prop_partner"))

# Define the OLS model formula object:
formula_obj <- count ~ time_ipc + prop_partner

# Plot the residual/datum checks:
RregressPkg::plot_residuals_check(
  df = sperm1_dt,
  formula_obj = formula_obj,
  id_col = "subject",
  residual_label_threshold = 120,
  leverage_label_threshold = 0.2,
  label_sd = 1.0,
  title = "Check Residuals",
  subtitle = "count ~ time_ipc + prop_partner",
  pts_size = 2.5,
  pts_fill = "green",
  pts_color = "black"
)
