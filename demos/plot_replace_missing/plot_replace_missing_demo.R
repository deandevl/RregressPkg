library(ggplot2)
library(data.table)
library(rlang)
library(RplotterPkg)
library(RregressPkg)
library(mlbench)

data("Soybean", package = "mlbench")

str(Soybean)

# The class of the measurement variables in Soybean is "factor" or "ordered".
# Note: the first column is character and is ignored.
# Convert them to numeric:
for(i in 2:ncol(Soybean)){
  Soybean[,i] <- as.numeric(Soybean[,i])
}

# Set the columns of interest:
columns <- colnames(Soybean)[2:ncol(Soybean)]

# "date" variable has one missing value. For testing assign
# 2 missing values to "date" variable for a total of 3.
# "leaves" variable has no missing values. For testing assign
# 5 missing values to the "leaves" variable for a total of 5.

Soybean$date[[3]] <- NA
Soybean$date[[4]] <- 99
Soybean$leaves[[4]] <-  NA
Soybean$leaves[[5]] <-  "N/A"
Soybean$leaves[[6]] <-  "na"
Soybean$leaves[[7]] <- NA
Soybean$leaves[[8]] <- NaN

# Apply RregressPkg::plot_replace_missing() where by default,
#  missing values for a variable are replaced by its median.
soybean_missing <- RregressPkg::plot_replace_missing(
  df = Soybean,
  variables = columns,
  miss_values = c("N/A", "na", 99),
  title = "Count of Missing Values",
  subtitle = "mlbench::Soybean data set",
  x_title = "Variable",
  y_title = "Count of Missing Values",
  bar_size = 0.6,
  bar_color = "white",
  bar_labels = TRUE,
  bar_label_size = 3,
  do_coord_flip = TRUE,
  order_bars = "asc"
)
soybean_missing$missing_plot

# Check for missing values again on the returned replaced values data frame
soybean_missing_replaced <- RregressPkg::plot_replace_missing(
  df = soybean_missing$replacement_df,
  variables = columns,
  miss_values = c("N/A", "na"),
  title = "Check on Count of Missing Values",
  subtitle = "mlbench::Soybean data set",
  x_title = "Variable",
  y_title = "Count of Missing Values",
  bar_size = 0.6,
  bar_color = "white",
  bar_labels = TRUE,
  bar_label_size = 3,
  do_coord_flip = TRUE,
  y_limits = c(0,125),
  y_major_breaks = seq(0,125,25)
)
soybean_missing_replaced$missing_plot

# Apply RregressPkg::plot_replace_missing() again where a function
# is used to set a missing value. The following function just returns
# NA for all missing values found. Note the "x" in the function will
# receive a numeric vector of non-missing values for a specific
# column/variable which could be used in calculating a replacement
# value for missing values in the column.
missing_val_fun <- function(x){
  return(NA_real_)
}
soybean_missing <- RregressPkg::plot_replace_missing(
  df = Soybean,
  variables = columns,
  miss_values = c("N/A", "na", 99),
  replace_fun = missing_val_fun,
  title = "Count of Missing Values",
  subtitle = "mlbench::Soybean data set",
  bar_size = 0.6,
  bar_color = "white",
  bar_labels = TRUE,
  bar_label_size = 3,
  do_coord_flip = TRUE,
  order_bars = "asc"
)
soybean_missing$missing_plot