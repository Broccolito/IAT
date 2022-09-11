library(dplyr)
library(purrr)

library(ggplot2)
library(ggpubr)

rm(list = ls())
load("pitch_data.rda")

plt = ggplot(data = pitch_data, aes(x = condition, y = pitch_angle)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", size = 2, shape = 21,
             position = position_dodge2(0.2)) + 
  labs(fill = "Condition") + 
  stat_compare_means(method = "t.test", comparisons = list(c("diseased", "normal"))) + 
  xlab("") + 
  ylab("Pitch Angle (degree)") + 
  theme_pubr()

ggsave(filename = "figures/pitch_angle.png", plot = plt, dpi = 1200, width = 4, height = 5)
