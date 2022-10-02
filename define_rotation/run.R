library(dplyr)
library(ggplot2)
library(ggpubr)
library(sp)
library(spData)
library(spdep)
library(purrr)
library(writexl)

source("find_theta.R")

diseased_paths = c(
  paste0("image_data/diseased/batch1/", list.files(path = "image_data/diseased/batch1/", pattern = ".txt")),
  paste0("image_data/diseased/batch2/", list.files(path = "image_data/diseased/batch2/", pattern = ".txt"))
)

diseased_filenames = c(
  list.files(path = "image_data/diseased/batch1/", pattern = ".txt"),
  list.files(path = "image_data/diseased/batch2/", pattern = ".txt")
)

control_paths = c(
  paste0("image_data/normal/", list.files(path = "image_data/normal/", pattern = ".txt"))
)

control_filenames = list.files(path = "image_data/normal/", pattern = ".txt")

condition = c(
  rep("diseased", length(diseased_filenames)),
  rep("control", length(control_filenames))
)

all_paths = c(diseased_paths, control_paths) %>%
  as.list()
all_filenames = c(diseased_filenames, control_filenames)

results = map(all_paths, find_theta, insertion_peak_index = 90) 
result_mat = vector()
for(i in 1:length(results)){
  try({
    result_mat = rbind.data.frame(
      result_mat,
      tibble(
        optimum_insertion_angle = results[[i]][["Optimum Insertion Angle"]],
        original_pitch_angle = results[[i]][["Original Pitch Angle"]],
        rotated_pitch_angle = results[[i]][["Rotated Pitch Angl"]],
        delta_pitch_angle = results[[i]][["Delta Pitch Angle"]],
        ratio_10_14_8_9 = results[[i]][["Ratio 10_14/8_9"]],
        ratio_8_9_4_7 = results[[i]][["Ratio 8_9/4_7"]],
        curve_length_ratio = results[[i]][["Curve Length Ratio"]]
      )
    )
  })
}

result_mat = result_mat %>%
  mutate(filenames = all_filenames) %>%
  mutate(condition = condition) %>%
  select(filenames, condition, everything())

write_xlsx(result_mat, path = "wb90_measurements.xlsx")
