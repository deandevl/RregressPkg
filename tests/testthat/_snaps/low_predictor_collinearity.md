# low_predictor_collinearity()

    Code
      bloodpress_predictors_dt <- RregressPkg::bloodpress[, !c("Pt", "BP")]
      RregressPkg::low_predictor_collinearity(df = bloodpress_predictors_dt)
    Output
      $predictors
      [1] "Age"    "BSA"    "Dur"    "Pulse"  "Stress"
      
      $correlations
                   Age        BSA       Dur     Pulse     Stress
      Age    1.0000000 0.37845460 0.3437921 0.6187643 0.36822369
      BSA    0.3784546 1.00000000 0.1305400 0.4648188 0.01844634
      Dur    0.3437921 0.13054001 1.0000000 0.4015144 0.31163982
      Pulse  0.6187643 0.46481881 0.4015144 1.0000000 0.50631008
      Stress 0.3682237 0.01844634 0.3116398 0.5063101 1.00000000
      
      $max_correlation
      [1] 0.6187643
      

