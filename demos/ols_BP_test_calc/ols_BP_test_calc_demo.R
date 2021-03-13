library(data.table)
library(RregressPkg)
library(wooldridge)

# set up the data set
#Source: "Using R for Introductory Econometrics" by Florian Heiss (page 147)
# Also: "Introductory Econometrics" by jeffery Wooldridge (page 251)
data("hprice1", package = "wooldridge")
hprice1_dt <- data.table::setDT(hprice1)
hprice1_dt <- hprice1[, .(price, lotsize, sqrft, bdrms)]

formula_obj <- price ~ lotsize + sqrft + bdrms
BP_test_df <- RregressPkg::ols_BP_test_calc(
  df = hprice1_dt,
  formula_obj = formula_obj
)

# use logs
log_formula_obj <- log(price) ~ log(lotsize) + log(sqrft) + bdrms
BP_test_log_df <- RregressPkg::ols_BP_test_calc(
  df = hprice1_dt,
  formula_obj = log_formula_obj
)
