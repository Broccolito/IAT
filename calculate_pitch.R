library(dplyr)
library(purrr)

rm(list = ls())
load("data.rda")

pitch_data = data %>%
  split(.$file_name) %>%
  map(function(dff){
    p13 = filter(dff, index == 13)
    condition = dff$condition[1]
    file_name = dff$file_name[1]
    pitch_angle = atan2(p13$y, -p13$x) * (180/pi)
    return(
      tibble(condition, file_name, pitch_angle)
    )
  }) %>%
  reduce(rbind.data.frame)

