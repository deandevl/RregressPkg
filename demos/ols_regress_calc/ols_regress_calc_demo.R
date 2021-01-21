# Title     : ols_regress_calc_demo.R
# Objective : demonstrates ols_regress_calc() function
# Created by: Rick Dean
# Created on: 2021-01-09 11:34 AM

library(data.table)
library(RregressPkg)
library(here)

# --------------------Single Regression: Mort ~ Lat---------------------
# Mort: Mortality Rate (number of deaths per 10 million people) of white males (1950-1959)
# Lat: Latitude (degrees North) at the center of each of 49 states in the US
# Source: PennState Stat 462 Eberly College of Science
current_dir <- here::here()
data_path <- file.path(current_dir, "demos/data/skin_cancer.txt")
cancer_data_dt <- data.table::fread(data_path)
cancer_ols_ls <- RregressPkg::ols_regress_calc(
  data_df = cancer_data_dt[, .(Mort, Lat)],
  dep_str = "Mort"
)

# -------------------Multiple Regression: colGPA ~ hsGPA + ACT------------------------
# colGPA: College GPA;  hsGPA: High School GPA;  ACT: ACT score
# Source: "Using R for Introductory Econometrics" by Florian Heiss, page 92
# Also: "Introductory Econometrics, A Modern Approach, 6th edition" by Jeffery Wooldridge, page 71
library(wooldridge)
data("gpa1")
str(gpa1)

gpa1_df <- data.frame(
  colGPA = gpa1$colGPA,
  hsGPA = gpa1$hsGPA,
  ACT = gpa1$ACT
)

gpal_ols_ls <- RregressPkg::ols_regress_calc(
  data_df = gpa1_df,
  dep_str = "colGPA"
)



