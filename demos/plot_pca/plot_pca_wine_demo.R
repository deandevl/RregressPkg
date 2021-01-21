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

# read the wine data
data_path <- file.path(current_dir, "demos/data/wine.data")
wine_df <- read.table(data_path, sep = ",")
measurements <- c("Alcohol","Malic acid","Ash","Alcalinity of ash",
                  "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                  "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
colnames(wine_df) <- c("Class",measurements)

wine_df <- wine_df %>%
  mutate(Class = as.factor(wine_df$Class))

# plot PC1 vs PC2
wine_pca_plots <- RregressPkg::plot_pca(
  df = wine_df,
  center = T,
  scale. = T,
  rank. = 4,
  measures = measurements,
  aes_fill = "Class",
  pts_size = 2,
  palette_colors = c("red","blue","green"),
  x_limits = c(-5, 5),
  x_major_breaks = seq(-5, 5, 1),
  y_limits = c(-5,5),
  y_major_breaks = seq(-5,5,1),
  title = "Principal Components of Wine",
  subtitle = "13 attributes from 178 samples of 3 classes",
  figure_width = 12
)

