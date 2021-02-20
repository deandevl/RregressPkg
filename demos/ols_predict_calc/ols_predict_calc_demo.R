library(RregressPkg)
library(wooldridge)
library(here)

current_dir <- here::here()
# single estimate - "confidence interval"
data_path <- file.path(current_dir, "demos/data/iqsize.txt")
iqsize_dt <- data.table::fread(data_path)
iqsize_dt <- iqsize_dt[, !c("Weight")]

predictors_df <- data.frame(
  Brain = 90.0,
  Height = 70.0
)

iq_formula <- PIQ ~ Brain + Height

iqsize_respones_df <- RregressPkg::ols_predict_calc(
  df = iqsize_dt,
  formula_obj = iq_formula,
  predictors_df = predictors_df
)

# single estimate - "prediction interval"
iqsize_respones_df <- RregressPkg::ols_predict_calc(
  df = iqsize_dt,
  formula_obj = iq_formula,
  predictors_df = predictors_df,
  interval = "prediction"
)

# ------------------Wooldridge::gpa2------------------
# single estimate - "confidence interval"
data("gpa2", package = "wooldridge")

predictors_df <- data.frame(
  sat = 1200,
  hsperc = 30,
  hsize = 5
)

gpa2_formula <- colgpa ~ sat + hsperc + hsize + I(hsize^2)

gpa2_response_df <- RregressPkg::ols_predict_calc(
  df = gpa2,
  formula_obj = gpa2_formula,
  predictors_df = predictors_df
)

# ------------------Wooldridge::gpa2------------------
# multiple estimates -- "confidence interval" at 99%
predictors_df <- data.frame(
  sat = c(1200, 900, 1400),
  hsperc = c(30, 20, 5),
  hsize = c(5, 3, 1)
)

gpa2_response_df <- RregressPkg::ols_predict_calc(
  df = gpa2,
  formula_obj = gpa2_formula,
  predictors_df = predictors_df,
  confid_level = 0.99
)

# ----------------Wooldridge::hprice2  "confidence interval"-----------------
data("hprice2", package = "wooldridge")

predictors_df <- data.frame(
  rooms = c(4,5,6,7,8),
  nox = rep(5.5498,5),
  dist = rep(3.7958,5),
  stratio = rep(18.4593,5)
)
hprice2_formula <- log(price) ~ log(nox) + log(dist) + rooms + I(rooms^2) + stratio
hprice2_response_df <- RregressPkg::ols_predict_calc(
  df = hprice2,
  formula_obj = hprice2_formula,
  predictors_df = predictors_df
)
