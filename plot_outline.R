library(dplyr)
library(circular)
library(purrr)

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(plotly)

rm(list = ls())
load("data.rda")

plt = ggplot(data = data, aes(x = x, y = y, group = file_name)) +
  geom_point(aes(color = condition)) +
  labs(color = "Condition") + 
  facet_grid(condition~.) + 
  theme(text = element_text(size = 15)) +
  theme_pubr(); plt

ggsave(filename = "figures/outline.png", plot = plt, device = "png",
       dpi = 1200, width = 5, height = 6)
