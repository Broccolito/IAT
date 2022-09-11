library(dplyr)
library(circular)
library(purrr)

rm(list = ls())
load("data.rda")

data = data %>%
  filter(index >= 36) %>%
  filter(x > -0.1)

circle_data = data %>%
  split(.$file_name) %>%
  map(function(dff){
    circle_fit = lsfit.circle(x=dff$x, y=dff$y)
    radius = circle_fit$coefficients[1]
    a = circle_fit$coefficients[2]
    b = circle_fit$coefficients[3]
    condition = dff$condition[1]
    return(tibble(file_name = dff$file_name[1],
                  condition = dff$condition[1],
                  radius = radius,
                  a, b))
  }) %>%
  reduce(rbind.data.frame)

t.test(radius ~ condition, circle_data)

save(circle_data, file = "circle_data.rda")





