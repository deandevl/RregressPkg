library(dplyr)
library(grid)
library(gtable)
library(rlang)
library(ggplot2)
library(ggrepel)
library(here)
library(RplotterPkg)
library(RregressPkg)

current_dir <- here::here()

# read the pilot data
data_path <- file.path(current_dir, "demos/data/pilot.data")
pilots <- read.table(data_path, sep = "")
measurements <- c("Intelligence", "Form Relations", "Dynamometer", "Dotting", "Coordination", "Perservation")
colnames(pilots) <- c("Group", measurements)

# create a data frame of scaled measurements and pilot groups
pilots_df <- as.data.frame(pilots[2:7])
pilots_df <- pilots_df %>%
  mutate(Group = ifelse(pilots$Group == 1, "Apprentice", "Pilot"))

pilots_pca_plots <- RregressPkg::plot_pca(
  df = pilots_df,
  measures = measurements,
  center = T,
  scale. = T,
  rank. = 4,
  aes_fill = "Group",
  pts_size = 2,
  palette_colors = c("red","blue"),
  x_limits = c(-4, 2),
  x_major_breaks = seq(-4, 2, 1),
  title = "Principal Components of Pilots and Apprentices",
  subtitle = "6 tested attributes from 20 pilots and 20 apprentices",
  figure_width = 12
)


