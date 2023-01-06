library(data.table)
library(ggplot2)
library(magrittr)
library(mlbench)
library(RplotterPkg)
library(RregressPkg)

# -------------------Simple example-----------------------------

# Create the data.frame with NA's and NaN's and other values considered missing:

set.seed(5648)
missing_dt <- data.table(
  x = 1:20,
  y = LETTERS[1:20],
  z = rnorm(20, mean = 10, sd = 2.1)
)
missing_dt$x[1:2] <- NA
missing_dt$x[[5]] <- 9999
missing_dt$x[[8]] <- 1111
missing_dt$x[12:15] <- NaN
missing_dt$z[8:9] <- NaN
missing_dt$z[18:19] <- NA

# Show the counts of missing values:
missing_lst <- RregressPkg::plot_replace_missing(
  df = missing_dt,
  variables = c("x", "z"),
  miss_values = c(1111, 9999),
  title = "Counts of Missing Values"
)
missing_lst$missing_plot

# Replace missing values with the mean value for the variable:
missing_lst <- RregressPkg::plot_replace_missing(
  df = missing_dt,
  variables = c("x", "z"),
  miss_values = c(1111, 9999),
  replace_fun = "median"
)

# Show both the original data and the data replaced:
replaced_dt <- missing_lst$replacement_df %>%
  data.table::setnames(old = c("x","y","z"), new = c("new_x","new_y","new_z"))
all_dt <- cbind(missing_dt, replaced_dt)

RplotterPkg::create_table(
  x = all_dt,
  source_note = "Both original and replaced data",
  container_width_px = 400
)

# ----------------------------Soybean example----------------------

# There are 683 observations and 36 variables.
# The class of the measurement variables in Soybean is either "factor" or "ordered".
# We will ignore the first variable "Class" since it is character type and convert
# all others to numeric.

data("Soybean", package = "mlbench")
for(i in 2:ncol(Soybean)){
  Soybean[,i] <- as.numeric(Soybean[,i])
}

# Set the columns of interest and insert some missing values.
# "date" variable has one missing value. For testing assign 2 additional
# missing values to "date" variable for a total of 3.
# "leaves" variable has no missing values. For testing assign 5 missing
# values to the "leaves" variable for a total of 5.

columns_of_interest <- colnames(Soybean)[2:ncol(Soybean)]
Soybean$date[[3]] <- NA
Soybean$date[[4]] <- 99
Soybean$leaves[[4]] <-  NA
Soybean$leaves[[5]] <-  "N/A"
Soybean$leaves[[6]] <-  "na"
Soybean$leaves[[7]] <- NA
Soybean$leaves[[8]] <- NaN

# Get a count of the missing values:
soybean_missing_lst <- RregressPkg::plot_replace_missing(
  df = Soybean,
  variables = columns_of_interest,
  miss_values = c("N/A", "na", 99),
  title = "Count of Missing Values",
  subtitle = "mlbench::Soybean data set",
  x_title = "Variable",
  y_title = "Count of Missing Values",
  bar_size = 0.6,
  bar_color = "white",
  bar_labels = TRUE,
  bar_label_sz = 3,
  do_coord_flip = TRUE,
  order_bars = "asc"
)
soybean_missing_lst$missing_plot

# Apply a user defined function for replacing missing values:
missing_val_fun <- function(x){
  xx <- as.numeric(x)
  return((max(xx) - min(xx))/2)
}

soybean_missing_lst <- RregressPkg::plot_replace_missing(
  df = Soybean,
  variables = columns_of_interest,
  replace_fun = missing_val_fun,
  miss_values = c("N/A", "na", 99),
  title = "Count of Missing Values",
  subtitle = "mlbench::Soybean data set",
  x_title = "Variable",
  y_title = "Count of Missing Values",
  bar_size = 0.6,
  bar_color = "white",
  bar_labels = TRUE,
  bar_label_sz = 3,
  do_coord_flip = TRUE,
  order_bars = "asc"
)
date_leaves_dt <- soybean_missing_lst$replacement_df %>%
  .[1:6, .(date, leaves)]
RplotterPkg::create_table(
  x = date_leaves_dt,
  source_note = "Replacement values for 'date' and  'leaves'",
  container_width_px = 300
)
# Note that "date" variable now has a value 3 and "leaves" variable
# now has a value of 0.5 for their missing values.

# Run `RregressPkg::plot_replace_missing()` again on the replacement data.frame
# to check on missing values:

soybean_missing_lst <- RregressPkg::plot_replace_missing(
  df = soybean_missing_lst$replacement_df,
  variables = columns_of_interest,
  miss_values = c("N/A", "na", 99),
  title = "Count of Missing Values",
  subtitle = "mlbench::Soybean data set",
  x_title = "Variable",
  y_title = "Count of Missing Values",
  bar_size = 0.6,
  bar_color = "white",
  bar_labels = TRUE,
  bar_label_sz = 3,
  do_coord_flip = TRUE,
  order_bars = "asc",
  y_major_breaks = c(0,1,2)
)
soybean_missing_lst$missing_plot