library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(ggpubr)

d = read_xlsx(path = "IAT Measurements at 90.xlsx")

d = d %>%
  mutate(group = ifelse(group == "Diseased", "IAT", group))

d$group = factor(d$group, levels = c("Control", "IAT"))

# summary(filter(d, group == "Control")$optimum_insertion_angle)
# summary(filter(d, group == "IAT")$optimum_insertion_angle)

plt1 = ggplot(data = d, aes(x = group, y = optimum_insertion_angle)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(fill = group), color = "black", shape = 21, size = 3,  
             position = position_dodge2(0.2)) +
  scale_fill_manual(values = c("#3382CC", "#E57348")) + 
  labs(fill = "") + 
  xlab("") + 
  ylab("Optimum Insertion Angle (°)") + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("IAT", "Control")),
                     symnum.args = list(cutpoints = c(0, 0.05, Inf), 
                                        symbols = c("*", "NS"))) +
  theme_pubclean() + 
  theme(text = element_text(size = 15), legend.position = "top"); plt1

ggsave(filename = "../../figures/insertion_angle.png", plot = plt1, device = "png",
       dpi = 1200, width = 3.5, height = 5)
