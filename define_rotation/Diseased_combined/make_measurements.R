library(ggplot2)
library(ggpubr)
library(writexl)

source("find_theta.R")

get_iat_measurements = function(insertion_point_index = 90,
                                filename = "IAT Measurements at 90.xlsx"){
  
  control_files = list.files(path = "Control", pattern = "txt")
  diseased_files = list.files(path = "Diseased_combined/", pattern = "txt")
  
  control_measurements = vector()
  for(control_file in control_files){
    try({
      result = find_theta(pointset_filename = paste0("Control/", control_file), 
                          insertion_peak_index = insertion_point_index)
      optimum_insertion_angle = result$`Optimum Insertion Angle`
      original_pitch_angle = result$`Original Pitch Angle`
      rotated_pitch_angle = result$`Rotated Pitch Angle`
      delta_pitch_angle = result$`Delta Pitch Angle`
      ratio10_14over8_9 = result$`Ratio 10_14/8_9`
      ratio8_9over4_7 = result$`Ratio 8_9/4_7`
      insertion_length_ratio = result$`Curve Length Ratio`
      control_measurements = rbind.data.frame(control_measurements,
                                              tibble(
                                                group = "Control",
                                                file = control_file,
                                                optimum_insertion_angle,
                                                original_pitch_angle,
                                                rotated_pitch_angle,
                                                delta_pitch_angle,
                                                ratio10_14over8_9,
                                                ratio8_9over4_7,
                                                insertion_length_ratio
                                              )
      )
      rm(result)
    }, silent = TRUE)
  }
  
  diseased_measurements = vector()
  for(diseased_file in diseased_files){
    try({
      result = find_theta(pointset_filename = paste0("Diseased_combined/", diseased_file), 
                          insertion_peak_index = insertion_point_index)
      optimum_insertion_angle = result$`Optimum Insertion Angle`
      original_pitch_angle = result$`Original Pitch Angle`
      rotated_pitch_angle = result$`Rotated Pitch Angle`
      delta_pitch_angle = result$`Delta Pitch Angle`
      ratio10_14over8_9 = result$`Ratio 10_14/8_9`
      ratio8_9over4_7 = result$`Ratio 8_9/4_7`
      insertion_length_ratio = result$`Curve Length Ratio`
      diseased_measurements = rbind.data.frame(diseased_measurements,
                                               tibble(
                                                 group = "Diseased",
                                                 file = diseased_file,
                                                 optimum_insertion_angle,
                                                 original_pitch_angle,
                                                 rotated_pitch_angle,
                                                 delta_pitch_angle,
                                                 ratio10_14over8_9,
                                                 ratio8_9over4_7,
                                                 insertion_length_ratio
                                               )
      )
      rm(result)
    })
  }
  
  all_measurements = rbind.data.frame(control_measurements, 
                                           diseased_measurements) %>%
    na.omit()
  
  write_xlsx(all_measurements, path = filename)
  
  return(all_measurements)
  
}

make_boxplots = function(all_measurements){
  
  plt1 = ggplot(data = all_measurements, aes(x = group, y = optimum_insertion_angle)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  plt2 = ggplot(data = all_measurements, aes(x = group, y = original_pitch_angle)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  plt3 = ggplot(data = all_measurements, aes(x = group, y = rotated_pitch_angle)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  plt4 = ggplot(data = all_measurements, aes(x = group, y = delta_pitch_angle)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  plt5 = ggplot(data = all_measurements, aes(x = group, y = ratio10_14over8_9)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  plt6 = ggplot(data = all_measurements, aes(x = group, y = ratio8_9over4_7)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  plt7 = ggplot(data = all_measurements, aes(x = group, y = insertion_length_ratio)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(fill = group), color = "black", shape = 21, size = 4) +
    labs(fill = "") +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "Diseased")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns"))) +
    theme_pubr() +
    theme(text = element_text(size = 15))
  
  
  ggsave(filename = "optimum insertion angle.png", plot = plt1, device = "png", dpi = 1200)
  ggsave(filename = "original_pitch_angle.png", plot = plt2, device = "png", dpi = 1200)
  ggsave(filename = "rotated_pitch_angle.png", plot = plt3, device = "png", dpi = 1200)
  ggsave(filename = "delta_pitch_angle.png", plot = plt4, device = "png", dpi = 1200)
  ggsave(filename = "ratio10_14over8_9.png", plot = plt5, device = "png", dpi = 1200)
  ggsave(filename = "ratio8_9over4_7.png", plot = plt6, device = "png", dpi = 1200)
  ggsave(filename = "insertion length ratio.png", plot = plt7, device = "png", dpi = 1200)
  
}

measurement_at90 = get_iat_measurements(insertion_point_index = 90, filename = "IAT Measurements at 90.xlsx")
measurement_at12 = get_iat_measurements(insertion_point_index = 12, filename = "IAT Measurements at 12.xlsx")

# make_boxplots(measurement_at90)
# make_boxplots(measurement_at12)


