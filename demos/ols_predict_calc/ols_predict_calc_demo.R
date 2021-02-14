library(data.table)
library(RregressPkg)
library(wooldridge)
library(here)

current_dir <- here::here()
# single estimate
data_path <- file.path(current_dir, "demos/data/iqsize.txt")
iqsize_dt <- data.table::fread(data_path)
iqsize_dt <- iqsize_dt[, !c("Weight")]

predictors_df <- data.frame(
  Brain = 90.0,
  Height = 70.0
)

iqsize_respones_df <- RregressPkg::ols_predict_calc(
  x = iqsize_dt,
  resp_col = "PIQ",
  predictors_df = predictors_df
)

# ------------------Wooldridge::gpa2------------------
# single estimate
gpa_2_dt <- data.table::setDT(wooldridge::gpa2)
str(gpa_2_dt)
gpa_2_dt <- gpa_2_dt[, .(colgpa, sat, hsperc, hsize)]
gpa_2_dt[, hsize_sq := hsize^2]

predictors_df <- data.frame(
  sat = 1200,
  hsperc = 30,
  hsize = 5,
  hsize_sq = 25
)

gpa_2_response_df <- RregressPkg::ols_predict_calc(
  x = gpa_2_dt,
  confid_level = 0.95,
  resp_col = "colgpa",
  predictors_df = predictors_df
)

# ------------------Wooldridge::gpa2------------------
# multiple estimates -- prediction interval
predictors_df <- data.frame(
  sat = c(1200, 900, 1400),
  hsperc = c(30, 20, 5),
  hsize = c(5, 3, 1),
  hsize_sq = c(25, 9, 1)
)

gpa_2_response_df <- RregressPkg::ols_predict_calc(
  x = gpa_2_dt,
  interval = "prediction",
  confid_level = 0.99,
  resp_col = "colgpa",
  predictors_df = predictors_df
)




