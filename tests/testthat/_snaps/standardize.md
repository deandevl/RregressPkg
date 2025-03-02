# standardize()

    Code
      cols <- c("colGPA", "hsGPA", "ACT")
      dt <- data.table::as.data.table(wooldridge::gpa1)[, ..cols]
      stand_dt <- RregressPkg::standardize(df = dt, cols = c("hsGPA", "ACT"), mul = 2)
      head(stand_dt)
    Output
         colGPA        hsGPA         ACT
          <num>        <num>       <num>
      1:    3.0 -0.628470001 -0.55480823
      2:    3.4 -0.315897544 -0.02742872
      3:    3.0  0.309246998  0.32415762
      4:    3.5  0.152960955  0.49995079
      5:    3.6  0.778105869  0.67574396
      6:    3.0 -0.003325087  0.14836445

