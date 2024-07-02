library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(ggpubr)

load("example_theta.rda")

plt1 = ggplot(data = theta$Loss, aes(x = angle, y = loss)) + 
  geom_line(size = 1) + 
  xlab("Insertion Angle (degrees)") + 
  ylab("Rotational Loss (A.U.)") +
  ggtitle(label = NULL, 
          subtitle = paste0("Optimum Insertion Angle: ", 
                            round(theta$`Optimum Insertion Angle`,1),
                            " degrees")) + 
  geom_vline(aes(xintercept = round(theta$`Optimum Insertion Angle`,1)), linetype = "dashed") + 
  theme_pubr() +
  theme(text = element_text(size = 15))

# ggsave(filename = "../figures/loss_plot.png", plot = plt1, device = "png", dpi = 1200,
#        height = 4, width = 4)




