library(data.table)
library(magrittr)
library(here)
library(ggrepel)
library(ggplot2)
library(grid)
library(RplotterPkg)
library(RregressPkg)

# -------------------Principal Components of pilots-------------

# Data Reference: Travers, R. M. W. "The Use of a Discriminant Function
# in the Treatment of Psychological Group Differences," Psychometrika.

# R scripts inspired by "Principal Component Analysis with R Example"(https://aaronschlegel.me/principal-component-analysis-r-example.html)

## Read in the data:
data_path <- file.path(here::here(), "data/pilot.data")
pilots <- utils::read.table(data_path, sep = "")
measurements <- c("Intelligence", "Form Relations", "Dynamometer", "Dotting", "Coordination", "Perservation")
colnames(pilots) <- c("Group", measurements)

# Create a data frame of measurements and pilot groups:
pilots_dt <- data.table::as.data.table(pilots) %>%
  .[, Group := ifelse(pilots$Group == 1, "Apprentice", "Pilot")]

# Call RregressPkg::plot_pca() with scaling of the raw measurements:
pilots_pca_plots <- RregressPkg::plot_pca(
  df = pilots_dt,
  measures = measurements,
  center = T,
  scale. = T,
  rank. = 4,
  aes_fill = "Group",
  pts_size = 2,
  x_limits = c(-4, 2),
  x_major_breaks = seq(-4, 2, 1),
  title = "Principal Components of Pilots and Apprentices",
  subtitle = "6 tested attributes from 20 pilots and 20 apprentices",
  figure_width = 8
)

# Plot the percentage of explained variance by the components (Scree plot):
plot_df <- data.frame(
  component = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  variance = pilots_pca_plots$percent_var
)
RplotterPkg::create_bar_plot(
  df = plot_df,
  aes_x = "component",
  aes_y = "variance",
  title = "Principal Components of Pilot Traits",
  subtitle = "Components percent of explained variance",
  x_title = "Component",
  y_title = "Percent of Explained Variance",
  rot_y_tic_label = T,
  bar_fill = "blue"
)

# ----------------------Principal Components of wine-------------

# Data Reference: "Wine Data Set"(https://archive.ics.uci.edu/ml/datasets/Wine)
#
# R script inspired by "Principal Component Analysis in R, R-Bloggers"(https://www.r-bloggers.com/2017/01/principal-component-analysis-in-r/)

# Read in the data:
data_path <- file.path(here::here(), "data/wine.data")
wine_dt <- data.table::as.data.table(read.table(data_path, sep = ","))

# Create a data table with new column names; set "Class" column as factor:
old_names <- colnames(wine_dt)
new_names <- c("Class","Alcohol","Malic acid","Ash","Alcalinity of ash",
                  "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                  "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

wine_dt <- wine_dt %>%
  data.table::setnames(old = old_names, new = new_names) %>%
  .[, Class := as.factor(Class)]

# Call RregressPkg::plot_pca() with scaling of the raw measurements:
wine_pca_plots <- RregressPkg::plot_pca(
  df = wine_dt,
  center = T,
  scale. = T,
  rank. = 5,
  measures = new_names[2:14],
  aes_fill = "Class",
  pts_size = 2,
  x_limits = c(-5, 5),
  x_major_breaks = seq(-5, 5, 1),
  y_limits = c(-5,5),
  y_major_breaks = seq(-5,5,1),
  title = "Principal Components of Wine",
  subtitle = "13 attributes from 178 samples of 3 classes",
  figure_width = 12
)

# Plot the percentage of explained variance by the components (Scree plot):
plot_df <- data.frame(
  component = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13"),
  variance = wine_pca_plots$percent_var
)
RplotterPkg::create_bar_plot(
  df = plot_df,
  aes_x = "component",
  aes_y = "variance",
  title = "Principal Components of Wine Traits",
  subtitle = "Components percent of explained variance",
  x_title = "Component",
  y_title = "Percent of Explained Variance",
  rot_y_tic_label = T,
  bar_fill = "blue",
  order_bars = "desc"
)

