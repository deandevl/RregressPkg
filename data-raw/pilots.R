library(here)
library(usethis)
library(data.table)

data_file_path <- file.path(here::here(), "data-raw", "pilot.data")
usethis::use_data(pilot, overwrite = TRUE)
