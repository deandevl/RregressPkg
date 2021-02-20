library(RplotterPkg)
library(RregressPkg)
library(data.table)
library(ggplot2)
library(wooldridge)

data("hprice2", package = "wooldridge")

hprice2_dt <- data.table::setDT(hprice2)
hprice2_dt <- hprice2_dt[, .(price, nox, dist, rooms, stratio)]

response_lst <- RregressPkg::plot_predictor(
  df = hprice2_dt,
  formula_obj = log(price) ~ log(nox)+log(dist)+rooms+I(rooms^2)+stratio,
  predictor_df =  data.frame(rooms = seq(4, 8, 0.1)),
  title = "Influence of 'rooms' on 'log(price)'",
  subtitle = "Formula: log(price) ~ log(nox) + log(dist) + rooms + rooms^2 + stratio",
  x_title = "rooms",
  y_title = "log(price)"
)
response_lst$plot

