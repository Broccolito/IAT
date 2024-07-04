library(dplyr)
library(circular)
library(purrr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(plotly)

rm(list = ls())
load("data.rda")
load("circle_data.rda")

data = filter(data, condition == "normal")

data = data %>%
  filter(!(index %in% c(1,2,3))) %>%
  mutate(marker = ifelse(index %in% c(4,5,6,7, 8:14), "Marker", ifelse(index %in% 15:36, "Superior", "Insertion")))

data$marker = factor(data$marker, levels = c("Marker", "Superior", "Insertion"))
data$x = data$x + 0.7444787

average_radius = mean(circle_data$radius)
average_a = mean(circle_data$a) + 0.7444787
average_b = mean(circle_data$b)

create_circle = function(center, radius, npoints = 100) {
  angles = seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = center[1] + radius * cos(angles),
    y = center[2] + radius * sin(angles)
  )
}

# Circle parameters
circle_center = c(0.5186329, 0.4003364)
circle_radius = 0.4774087

# Create the circle
circle_data = create_circle(circle_center, circle_radius)

plt = ggplot(data = data, aes(x = x, y = y)) +
  geom_point(aes(fill = marker), shape = 22, color = "black", size = 2) + 
  scale_fill_manual(values = c("gray", "#619CFF", "#F8766D")) + 
  labs(fill = "") + 
  labs(color = "Condition") + 
  xlab("X") + ylab("Y") + 
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, alpha = 0.2, color = "black") +
  geom_path(data = circle_data, aes(x = x, y = y), color = "black", linetype = "solid") + 
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top"); plt


ggsave(filename = "figures/outline.png", plot = plt, device = "png",
       dpi = 1200, width = 6, height = 6, bg = "white")
