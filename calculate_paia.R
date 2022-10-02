library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(sp)
library(spData)
library(spdep)

load("circle_data.rda")
load("data.rda")

circle_data = circle_data %>%
  filter(condition == "normal")

# a = -0.215 +/- 0.074
# b = 0.387+/-0.034
# r = 0.466+/-0.064


data$x[data$condition == "normal" & data$index == 7]
x_o_scale = mean(1-(data$x[data$condition == "normal" & data$index == 6]-circle_data$a))
y_o_scale = mean(circle_data$b)
