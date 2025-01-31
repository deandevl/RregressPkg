# ols_restricted_model()

    Code
      mlb1_dt <- data.table::as.data.table(wooldridge::mlb1)[, .(salary, years,
        gamesyr, bavg, hrunsyr, rbisyr)]
      ur_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr + bavg +
        hrunsyr + rbisyr)
      r_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr)
      restricted_salary_lst <- RregressPkg::ols_restricted_model(df = mlb1_dt,
        ur_formula_obj = ur_formula_obj, r_formula_obj = r_formula_obj, confid_level = 0.99)
      restricted_salary_lst$F_df$F.Value
    Output
      [1] 9.550254
    Code
      restricted_salary_lst$F_df$Critical
    Output
      [1] 3.83852

