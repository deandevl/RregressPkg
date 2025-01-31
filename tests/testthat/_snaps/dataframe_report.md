# dataframe_report()

    Code
      set.seed(123)
      dt <- data.frame(id = 1:100, category = sample(c("A", "B", "C", NA), 100,
      replace = T), value = c(rnorm(97), -10, 100, NA), date = c(seq.Date(from = as.Date(
        "2020-01-01"), by = "day", length.out = 99), NaN), text = sample(c("Lorem",
        "Ipsum", "Dolor", "Sit", NA), 100, replace = T))
      RregressPkg::dataframe_report(dt)
    Output
      $datatypes
           column data_type
           <fctr>    <char>
      1:       id   integer
      2: category character
      3:    value   numeric
      4:     date      Date
      5:     text character
      
      $missing
           column missing_count
           <fctr>         <int>
      1:       id             0
      2: category            17
      3:    value             1
      4:     date             1
      5:     text            26
      
      $outliers
            id category value       date   text column    upper     lower
         <int>   <char> <num>     <Date> <char> <char>    <num>     <num>
      1:    98        B   -10 2020-04-07    Sit  value 2.218608 -2.403132
      2:    99        C   100 2020-04-08  Dolor  value 2.218608 -2.403132
      

