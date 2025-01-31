# ols_BP_test_calc()

    Code
      price_formula_obj = stats::as.formula("price~lotsize+sqrft+bdrms")
      RregressPkg::ols_BP_test_calc(df = wooldridge::hprice1, formula_obj = price_formula_obj)
    Output
      $statistic
      [1] 14.09239
      
      $p.value
      [1] 0.00278206
      

