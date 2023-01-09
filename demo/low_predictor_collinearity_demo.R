library(data.table)
library(here)
library(RplotterPkg)
library(RregressPkg)

# The following R script demonstrates the function RregressPkg::low_predictor_collinearity() 
# for assistance in selecting predictors in an OLS model.  The data set of predictors originates 
# from [PennState Eberly College of Science](https://online.stat.psu.edu/stat501/lesson/welcome-stat-501) 
# Lesson 12: Multicollinearity & Other Regression Pitfalls.

# Read in the data set.
data_path <- file.path(here::here(), "demo/data/bloodpress.txt")
bloodpress_dt <- data.table::fread(data_path)
bloodpress_dt <- bloodpress_dt[, !c("Pt")]

# Show the correlations among the predictors and the blood pressure response variable.
bp_corr_lst <- RregressPkg::plot_correlations(
  x = bloodpress_dt,
  title = "Blood Pressure Related Variables",
  subtitle = "PennState Stat 501 Eberly College of Science"
)

# From a plot
bp_corr_lst$corr_plot

# From a table
row_names_df <- data.frame(
  Variable = row.names(bp_corr_lst$corr)
) 

bloodpress_corr_df <- cbind(row_names_df, as.data.frame(bp_corr_lst$corr))
RplotterPkg::create_table(
  x = bloodpress_corr_df,
  container_width_px = 400,
  source_note = "Figure 1: Correlations among the predictors and the blood pressure response variable."
)

# Note the high correlation between *Weight* and *BSA* at .88. 
# Also *Weight* and *Pulse* have a correlation of .66.

# Identify highly correlated predictors.
# Call RregressPkg::low_predictor_collinarity() with the data frame of predictors.
# Using the default of .75 for the threshold correlation, show the selected predictors 
# for high blood pressure.

bloodpress_predictors_dt <- bloodpress_dt[, !c("BP")]
low_collinearity_lst <- RregressPkg::low_predictor_collinearity(
  df = bloodpress_predictors_dt
)
low_collinearity_lst$predictors

# The function's algorithm has removed the *Weight* predictor.
