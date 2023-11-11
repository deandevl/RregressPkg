library(data.table)
library(RregressPkg)
library(magrittr)

set.seed(123)

dt <- data.table(
  id = 1:100,
  category = sample(c("A","B","C",NA), 100, replace = T),
  value = c(rnorm(97), -10, 100, NA),
  date = c(seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 99), NaN),
  text = sample(c("Lorem","Ipsum","Dolor","Sit",NA), 100, replace = T)
)

str(dt)

report_lst <- RregressPkg::dataframe_report(dt)


