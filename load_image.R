library(dplyr)
library(ggplot2)
library(plotly)

### Load image data from normal group
filelist = list.files(path = "image_data/normal/", pattern = "txt")
filepath = paste0("image_data/normal/", filelist)

fp = data.frame(
  filelist = filelist, 
  filepath = filepath
)

normal_data = vector()
for(i in 1:dim(fp)[1]){
  f = read.delim(file = fp$filepath[i], header = F)
  names(f) = c("x", "y")
  file_name = fp$filelist[i]
  f$index = 1:dim(f)[1]
  f$file_name = file_name
  f$condition = "normal"
  normal_data = rbind.data.frame(normal_data, f)
  rm(f)
}

### load image data from diseased group -- batch1
filelist = list.files(path = "image_data/diseased/batch1/", pattern = "txt")
filepath = paste0("image_data/diseased/batch1/", filelist)

fp_batch1 = data.frame(
  filelist = filelist,
  filepath = filepath
)

### load image data from diseased group -- batch2
filelist = list.files(path = "image_data/diseased/batch2/", pattern = "txt")
filepath = paste0("image_data/diseased/batch2/", filelist)

fp_batch2 = data.frame(
  filelist = filelist,
  filepath = filepath
)

diseased_data = vector()

for(i in 1:dim(fp_batch1)[1]){
  f = read.delim(file = fp_batch1$filepath[i], header = F)
  names(f) = c("x", "y")
  file_name = fp_batch1$filelist[i]
  f$index = 1:dim(f)[1]
  f$file_name = file_name
  f$condition = "diseased1"
  diseased_data = rbind.data.frame(diseased_data, f)
  rm(f)
}

for(i in 1:dim(fp_batch2)[1]){
  f = read.delim(file = fp_batch2$filepath[i], header = F)
  names(f) = c("x", "y")
  file_name = fp_batch2$filelist[i]
  f$index = 1:dim(f)[1]
  f$file_name = file_name
  f$condition = "diseased2"
  diseased_data = rbind.data.frame(diseased_data, f)
  rm(f)
}


### Combine the normal and diseased data
data = rbind.data.frame(normal_data, diseased_data)



get_distance = function(p1, p2){
  ((p1$x - p2$x)^2 + (p1$y - p2$y)^2)^0.5
} 

data_normalized = vector()
for(f in unique(data$file_name)){
  
  file_data = filter(data, file_name == f) %>%
    filter(!(index %in% c(1, 2, 3)))
  
  index6_position = filter(file_data, index == 6)[,1:2]
  index7_position = filter(file_data, index == 7)[,1:2]
  index5_position = filter(file_data, index == 5)[,1:2]
  
  distance67 = get_distance(index6_position, index7_position)
  distance56 = get_distance(index5_position, index6_position)
  
  file_data$x = file_data$x - index6_position$x
  file_data$y = file_data$y - index6_position$y
  
  file_data$x = file_data$x/distance67
  file_data$y = -file_data$y/distance56
  
  data_normalized = rbind.data.frame(data_normalized,
                                     file_data)
}

data_normalized = data_normalized %>%
  mutate(condition = ifelse(condition != "normal", "diseased", "normal"))

ggplot(data = data_normalized, aes(x = x, y = y)) +
  geom_point(aes(color = condition)) + 
  facet_grid(condition~.)


