library(RregressPkg)
library(wooldridge)
library(data.table)

# set up the data set
data("mlb1", package = "wooldridge")
mlb1_dt <- data.table::setDT(mlb1)
mlb1_dt <- mlb1_dt[,.(salary, years, gamesyr, bavg, hrunsyr, rbisyr)]

ur_formula_obj <- log(salary) ~ years+gamesyr+bavg+hrunsyr+rbisyr
r_formula_obj <- log(salary) ~ years+gamesyr

# The following is a check
#ur_lm <- stats::lm(log(salary) ~ years+gamesyr+bavg+hrunsyr+rbisyr, data = mlb1_dt)
#ur_r2 <- summary(ur_lm)$r.squared

F_mlb1_df <- RregressPkg::ols_F_calc(
  df = mlb1,
  ur_formula_obj = ur_formula_obj,
  r_formula_obj = r_formula_obj
)


