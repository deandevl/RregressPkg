library(RregressPkg)
library(here)

# Check correlations among all variables - predictors and response
# Source: PennState Stat 501 Eberly College of Science, Section 12.1, Example 12.1

current_dir <- here::here()
data_dir <- file.path(current_dir, "demos/data")

# Read in the data
data_path <- file.path(data_dir, "bloodpress.txt")
bloodpress_dt <- data.table::fread(data_path)
bloodpress_dt <- bloodpress_dt[, !c("Pt")]

blood_correlations_df <- RregressPkg::compute_correlations(df = bloodpress_dt)
