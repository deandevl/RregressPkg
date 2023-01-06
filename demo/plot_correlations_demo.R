library(data.table)
library(ggplot2)
library(here)
library(magrittr)
library(RplotterPkg)
library(RregressPkg)

#  Create a data frame with 12 selected variables and 437 observations:
midwest_dt <- data.table::as.data.table(ggplot2::midwest) %>%
  .[, .(popdensity, percwhite, percblack, popadults, perchsd, percollege, percprof,
                             percbelowpoverty, percchildbelowpovert, percadultpoverty, percelderlypoverty, inmetro)]

# Plot the correlations without clustering, accepting all the plot defaults:
midwest_corr_lst <- RregressPkg::plot_correlations(
  x = midwest_dt,
  title = "Correlations of Midwest Demographic Variables",
  subtitle = "Source: ggplot2::midwest"
)
midwest_corr_lst$corr_plot


# Correlations related to blood pressure
# Source: PennState Stat 501 Eberly College of Science, Section 12.1, Example 12.1

data_path <- file.path(here::here(), "demo/data/bloodpress.txt")
bloodpress_dt <- data.table::fread(data_path) %>%
  .[, !c("Pt")]

#  Compute and show the correlations among the variables:
bp_corr_lst <- RregressPkg::plot_correlations(
  x = bloodpress_dt,
  title = "Blood Pressure Related Variables",
  subtitle = "PennState Stat 501 Eberly College of Science"
)
bp_corr_lst$corr_plot
