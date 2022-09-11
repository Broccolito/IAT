library(dplyr)
library(circular)
library(purrr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(plotly)

rm(list = ls())
load("circle_data.rda")

plt1 = ggplot(data = circle_data, aes(x = condition, y = radius)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", shape = 21, size = 2,  
             position = position_dodge2(0.2)) +
  labs(fill = "Condition") + 
  xlab("") + 
  ylab("Circle Radius (A.U.)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("diseased", "normal"))) +
  theme(text = element_text(size = 15)) + 
  theme_pubr()

plt2 = ggplot(data = circle_data, aes(x = condition, y = a)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", shape = 21, size = 2,  
             position = position_dodge2(0.2)) +
  labs(fill = "Condition") + 
  xlab("") + 
  ylab("Circle Horizontal Offset (A.U.)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("diseased", "normal"))) +
  theme(text = element_text(size = 15)) + 
  theme_pubr()

plt3 = ggplot(data = circle_data, aes(x = condition, y = b)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", shape = 21, size = 2,  
             position = position_dodge2(0.2)) +
  labs(fill = "Condition") + 
  xlab("") + 
  ylab("Circle Vertical Offset (A.U.)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("diseased", "normal"))) +
  theme(text = element_text(size = 15)) + 
  theme_pubr()


ggsave(filename = "figures/circle_radius.png", plot = plt1, device = "png",
       dpi = 1200, width = 4, height = 5)

ggsave(filename = "figures/horizontal_offset.png", plot = plt2, device = "png",
       dpi = 1200, width = 4, height = 5)

ggsave(filename = "figures/vertical_offset.png", plot = plt3, device = "png",
       dpi = 1200, width = 4, height = 5)
