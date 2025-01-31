library(here)
library(usethis)
library(data.table)

data_file_path <- file.path(here::here(), "data-raw", "bloodpress.txt")
bloodpress <- data.table::fread(file = data_file_path)

usethis::use_data(bloodpress, overwrite = TRUE)
