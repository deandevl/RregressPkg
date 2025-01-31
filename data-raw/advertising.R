library(here)
library(usethis)
library(data.table)

data_file_path <- file.path(here::here(), "data-raw", "Advertising.csv")
advertising <- data.table::fread(file = data_file_path)

usethis::use_data(advertising, overwrite = TRUE)
