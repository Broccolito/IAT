library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)

d = read_xlsx(path = "IAT Measurements at 90.xlsx")

d$group = factor(d$group, levels = c("Diseased", "Control"))

plt1 = ggplot(data = d, aes(x = group, y = optimum_insertion_angle)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = group), color = "black", size = 2, shape = 21,
             position = position_dodge2(0.2)) + 
  labs(fill = "Condition") + 
  stat_compare_means(method = "t.test", comparisons = list(c("Diseased", "Control"))) + 
  xlab("") + 
  ylab("Optimum Insertion Angle (degree)") + 
  theme_pubr()

plt2 = ggplot(data = d, aes(x = group, y = original_pitch_angle)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = group), color = "black", size = 2, shape = 21,
             position = position_dodge2(0.2)) + 
  labs(fill = "Condition") + 
  stat_compare_means(method = "t.test", comparisons = list(c("Diseased", "Control"))) + 
  xlab("") + 
  ylab("Pitch Angle (degree)") + 
  theme_pubr()

ggsave(filename = "insertion_angle.png", plot = plt1, dpi = 1200, width = 4, height = 5)
ggsave(filename = "pitch_angle.png", plot = plt2, dpi = 1200, width = 4, height = 5)



