# ols_calc()

    Code
      gpa1_dt <- data.table::as.data.table(wooldridge::gpa1)[, skipped := -skipped][,
        .(colGPA, hsGPA, ACT, skipped)]
      ols_ls <- RregressPkg::ols_calc(df = gpa1_dt, formula_obj = colGPA ~ hsGPA +
      ACT + skipped)
      ols_ls$coef_df
    Output
                         Coef      Value         SE  t_value      p_Value
      (Intercept) (Intercept) 1.38955383 0.33155354 4.191039 4.950269e-05
      hsGPA             hsGPA 0.41181617 0.09367422 4.396260 2.192050e-05
      ACT                 ACT 0.01472023 0.01056487 1.393319 1.657799e-01
      skipped         skipped 0.08311314 0.02599853 3.196840 1.725431e-03

