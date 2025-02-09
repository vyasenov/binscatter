rm(list=ls())

###################
################### LOAD LIBRARIES AND DATA
###################

library(ggplot2)
library(dplyr)
library(binsreg)

# Load dataset
data(iris)

###################
################### CREATE BINNED DATA
###################

# Define number of bins
bins <- 10

# Create binned dataset
iris_binned <- iris %>%
  mutate(bin = cut(Sepal.Length, breaks = bins, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(
    bin_mid = mean(range(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(bin)), ","))))),
    mean_petal_length = mean(Petal.Length)
  ) %>%
  ungroup()

###################
################### PLOT: RAW SCATTER & BINNED SCATTER
###################

# Create a combined dataset with a new column for panel distinction
iris_raw <- iris %>% 
  mutate(panel = "1. Raw Scatter Plot")
iris_binned <- iris_binned %>% 
  mutate(panel = "2. Binned Scatter Plot")

# Combine data
plot_data <- bind_rows(
  iris_raw %>% rename(x = Sepal.Length, y = Petal.Length),
  iris_binned %>% rename(x = bin_mid, y = mean_petal_length)
)

# Generate plot
fig <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~ panel, scales = "free_x", ncol = 2) +
  labs(
    title = "Comparison of Raw and Binned Scatter Plots",
    x = "Sepal Length",
    y = "Petal Length"
  ) +
  theme_minimal()

print(fig)

###################
################### SAVE
###################

ggsave(
  filename = "binscatter.png",  # File name
  plot = fig,                      # The plot object
  width = 10,                             # Width in inches
  height = 6,                             # Height in inches
  dpi = 300,                               # Resolution in dots per inch
  bg = 'white'
)

