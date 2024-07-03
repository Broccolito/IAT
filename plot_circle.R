library(dplyr)
library(circular)
library(purrr)
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(gridExtra)
library(plotly)

rm(list = ls())
load("circle_data.rda")

circle_data$a = circle_data$a + 0.7444787

circle_data = circle_data %>%
  mutate(condition = ifelse(condition == "diseased", "Diseased", "Control"))

plt1 = ggplot(data = circle_data, aes(x = condition, y = radius)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", shape = 21, size = 3,  
             position = position_dodge2(0.2)) +
  scale_fill_manual(values = c("#3382CC", "#E57348")) + 
  labs(fill = "") + 
  xlab("") + 
  ylab("Circle Radius (A.U.)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("Diseased", "Control")),
                     symnum.args = list(cutpoints = c(0, 0.05, Inf), 
                                        symbols = c("*", "NS"))) +
  theme_pubclean() + 
  theme(text = element_text(size = 15), legend.position = "top"); plt1


plt2 = ggplot(data = circle_data, aes(x = condition, y = a)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", shape = 21, size = 3,  
             position = position_dodge2(0.2)) +
  scale_fill_manual(values = c("#3382CC", "#E57348")) + 
  labs(fill = "") + 
  xlab("") + 
  ylab("Horizontal Offset (A.U.)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("Diseased", "Control")),
                     symnum.args = list(cutpoints = c(0, 0.05, Inf), 
                                        symbols = c("*", "NS"))) +
  theme_pubclean() + 
  theme(text = element_text(size = 15), legend.position = "top"); plt2

plt3 = ggplot(data = circle_data, aes(x = condition, y = b)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = condition), color = "black", shape = 21, size = 3,  
             position = position_dodge2(0.2)) +
  scale_fill_manual(values = c("#3382CC", "#E57348")) + 
  labs(fill = "") + 
  xlab("") + 
  ylab("Vertical Offset (A.U.)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("Diseased", "Control")),
                     symnum.args = list(cutpoints = c(0, 0.05, Inf), 
                                        symbols = c("*", "NS"))) +
  theme_pubclean() + 
  theme(text = element_text(size = 15), legend.position = "top"); plt3


ggsave(filename = "figures/circle_radius.png", plot = plt1, device = "png",
       dpi = 1200, width = 3.5, height = 5)

ggsave(filename = "figures/horizontal_offset.png", plot = plt2, device = "png",
       dpi = 1200, width = 3.5, height = 5)

ggsave(filename = "figures/vertical_offset.png", plot = plt3, device = "png",
       dpi = 1200, width = 3.5, height = 5)
