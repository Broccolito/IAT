library(ggplot2)
library(ggpubr)
library(knitr)

source("find_theta.R")

# Typical Case 1 "1906255R.txt"
pitch_original = find_theta("Typical/1906255R.txt", insertion_peak_index = 90)$`Rotated Pitch Angle`
pitch12 = find_theta("Typical/1906255R.txt", insertion_peak_index = 12)$`Rotated Pitch Angle`
pitch9 = find_theta("Typical/1906255R.txt", insertion_peak_index = 9)$`Rotated Pitch Angle`

kable(data.frame(pitch_original, pitch12, pitch9))

# | pitch_original|  pitch12|   pitch9|
# |--------------:|--------:|--------:|
# |       29.28267| 27.72151| 22.78142|


# Typical Case 2 "2590645.txt"
pitch_original = find_theta("Typical/2590645.txt", insertion_peak_index = 90)$`Rotated Pitch Angle`
pitch12 = find_theta("Typical/2590645.txt", insertion_peak_index = 12)$`Rotated Pitch Angle`
pitch9 = find_theta("Typical/2590645.txt", insertion_peak_index = 9)$`Rotated Pitch Angle`

kable(data.frame(pitch_original, pitch12, pitch9))

# | pitch_original|  pitch12|   pitch9|
# |--------------:|--------:|--------:|
# |       12.14995| 11.52732| 12.14995|
