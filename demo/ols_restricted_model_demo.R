library(data.table)
library(magrittr)
library(wooldridge)
library(RplotterPkg)
library(RregressPkg)

# The following R script demonstrates the function RregressionPkg::ols_restricted_model().
# Instead of single independent variable, groups of variables are evaluated for their
# effects on the dependent variable in an OLS model.

# ---------------Predicting baseball salaries and alternative OLS models------------

# Data source for this demo is "Using R for Introductory Econometrics" by Florian Heiss, Section 4.3 page 109
# and "Introductory Econometrics, A Modern Approach, 6th edition" by Jeffery Wooldridge, Section 4.5 page 127

# The text presents the following unrestricted model explaining major league baseball players' salaries:

# log(salary) = B0 + B1 years + B2 gamesyr +B3 bavg + B4 hrunsyr + B5 rbisyr + mu

# Our restricted model removes three independent variables with the following null hypothesis:

#                    H_0: B3 bavg = B4 hrnsyr + B5 rbisyr = 0
# Set up the data:
data("mlb1", package = "wooldridge")
mlb1_dt <- data.table::as.data.table(mlb1) %>%
  .[, .(salary, years, gamesyr, bavg, hrunsyr, rbisyr)]

# Define the unrestricted/restricted OLS models:
ur_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr)
r_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr)

# Call `RregressPkg::ols_restricted_model()`:
restricted_salary_lst <- RregressPkg::ols_restricted_model(
  df = mlb1_dt,
  ur_formula_obj = ur_formula_obj,
  r_formula_obj = r_formula_obj,
  confid_level = 0.99
)

# Show the F test and critical value for our null hypothesis H_0:
RplotterPkg::create_table(
  x = restricted_salary_lst$F_df,
  source_note = "F test for a restricted salary model",
  container_width_px = 400
)

# The F-value is well above the 1% critical value with 3 and 347 degrees of
# freedom, so we reject the hypothesis that bavg, hrunsyr, and rbisyr have no
# effect on salary.

# --------------------Alternative economic OLS models of crime-----------------

# Data source for this demo is "Using R for Introductory Econometrics" by Florian Heiss, Section 5.2 page 121
# and "Introductory Econometrics, A Modern Approach, 6th edition" by Jeffery Wooldridge, Section 5.2a page 158.

# The text presents the following unrestricted model explaining the number of times a man is arrested:
#
# narr86 = B0 + B1 pcnv + B2 avgsen + B3 tottime + B4 ptime86 + B5 qemp86 + mu

# where
#     narr86 = number of times a man was arrested
#     pcnv = proportion of prior arrests leading to a conviction
#     avgsen = average sentence served from past convictions
#     tottime = total time the man has spent in jail prior to 1986
#     ptime86 = months spent in prison in 1986
#     qemp86 = numbers of quarters in 1986 during which he was employed
# We want to test the null hypothesis that 'avgsen' and 'tottime' have no effect on 'narr86' once the other
# factors have been controlled for.

# Set up the data:
data("crime1", package = "wooldridge")
crime1_dt <- data.table::as.data.table(crime1) %>%
 .[, .(narr86, pcnv, avgsen, tottime, ptime86, qemp86)]

# Define the unrestricted/restricted OLS models:
ur_formula_obj <- stats::as.formula(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86)
r_formula_obj <- stats::as.formula(narr86 ~ pcnv + ptime86 + qemp86)

# Call 'RregressPkg::ols_restricted_model()' at the 10% level of significance:
restricted_arrest_lst <- RregressPkg::ols_restricted_model(
  df = crime1_dt,
  ur_formula_obj = ur_formula_obj,
  r_formula_obj = r_formula_obj,
  confid_level = 0.90
)

# Show the LM test and critical value for our null hypothesis H_0:
RplotterPkg::create_table(
  x = restricted_arrest_lst$LM_df,
  caption = "LM test for a restricted arrest model",
  container_width_px = 400
)
# From the Lagrange Multiplier (LM) statistic, we fail to reject
# the null hypothesis that 'avgsen' and 'tottime' = 0 at the 10% level.

# The F test also confirms failure to reject the null hypothesis:
RplotterPkg::create_table(
  x = restricted_arrest_lst$F_df,
  caption = "F test for a restricted arrest model",
  container_width_px = 400
)
