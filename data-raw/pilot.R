library(here)
library(usethis)
library(data.table)

data_file_path <- file.path(here::here(), "data-raw", "pilot.data")
pilots <- utils::read.table(data_file_path, sep = "")

measurements <- c(
  "Intelligence", "Form Relations",
  "Dynamometer", "Dotting",
  "Coordination", "Perservation")
colnames(pilots) <- c("Group", measurements)

pilots <- data.table::as.data.table(pilots) |>
  _[, Group := ifelse(pilots$Group == 1, "Apprentice", "Pilot")]

usethis::use_data(pilots, overwrite = TRUE)
