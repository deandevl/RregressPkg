library(data.table)
library(magrittr)
library(RregressPkg)
library(RplotterPkg)
library(wooldridge)

# The following R script demonstrates the function RregressionPkg::ols_calc() for estimating OLS model coefficients.

# ----------------------College GPA Model------------------------------
# Data source for this demo is from "Using R for Introductory Econometrics" by Florian Heiss, Section 4.1 page 105 and
# "Introductory Econometrics, A Modern Approach, 6th edition" by Jeffery Wooldridge, Section 4.2 page 115.

# The text presents the following model explaining college GPA (`colGPA`):

#colGPA = 1.39 + .412 hsGPA + .015 ACT - .083 skipped

#Predictors include:
#     `hsGPA`   high school grade point average
#     `ACT`     ACT score
#     `skipped` average number of lectures missed per week

# Set up the data:
data("gpa1", package = "wooldridge")
gpa1_dt <- data.table::as.data.table(gpa1) %>% 
  .[, skipped := -skipped] %>% 
  .[, .(colGPA, hsGPA, ACT, skipped)]

# Call `RregressPkg::ols_calc()`:
gpa1_ols_ls <- RregressPkg::ols_calc(
  df = gpa1_dt,
  formula_obj = stats::as.formula("colGPA ~ hsGPA + ACT + skipped")
)

# The coefficients:
RplotterPkg::create_table(
  x = gpa1_ols_ls$coef_df,
  source_note = "OLS for colGPA ~ hsGPA + ACT - skipped",
  container_width_px = 400
)

# The coefficients' confidence intervals:
RplotterPkg::create_table(
  x = gpa1_ols_ls$coef_CI_df,
  source_note = "Coefficient 95% Confidence Intervals",
  container_width_px = 400
)

# The F statistic:
RplotterPkg::create_table(
  x = gpa1_ols_ls$F_df,
  source_note = "F-statistic for colGPA ~ hsGPA + ACT + skipped",
  container_width_px = 500
)

# The ANOVA:
RplotterPkg::create_table(
  x = gpa1_ols_ls$anova_df,
  source_note = "ANOVA for OLS model colGPA ~ hsGPA + ACT + skipped",
  container_width_px = 500
)

# -------------------------Model of R&D Expenditures-------------------
# Source for this example is "Using R for Introductory Econometrics" by Florian Heiss,
# Section 4.2 page 108 and "Introductory Econometrics, A Modern Approach, 6th edition"
# by Jeffery Wooldridge, Section 4.3 page 123

# The text presents the following model explaining research and development spending based
# on annual sales(`sales`) and profits as a percentage of sales(`profmarg`):

# log(rd) = -4.38 + 1.084 log(sales) + .0217 profmarg

# Set up the data:
data("rdchem", package = "wooldridge")
rdchem_dt <- data.table::as.data.table(rdchem) %>%
  .[, .(rd, sales, profmarg)]

# Call `RregressPkg::ols_calc()`:
rdchem_ols_ls <- RregressPkg::ols_calc(
  df = rdchem_dt,
  formula_obj = stats::as.formula("log(rd) ~ log(sales) + profmarg")
)

# The coefficients:
RplotterPkg::create_table(
  x = rdchem_ols_ls$coef_df,
  source_note = "OLS for log(rd) ~ log(sales) + profmarg",
  container_width_px = 300
)

# The coefficients' confidence intervals:
RplotterPkg::create_table(
  x = rdchem_ols_ls$coef_CI_df,
  source_note = "Coefficient 95% Confidence Intervals",
  container_width_px = 400
)

# The F statistic:
RplotterPkg::create_table(
  x = rdchem_ols_ls$F_df,
  source_note = "F-statistic for log(rd) ~ log(sales) + profmarg",
  container_width_px = 500
)

# The ANOVA:
RplotterPkg::create_table(
  x = rdchem_ols_ls$anova_df,
  source_note = "ANOVA for OLS model log(rd) ~ log(sales) + profmarg",
  container_width_px = 500
)