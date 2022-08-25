library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotly)

get_distance = function(p1, p2){
  unlist(((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)^0.5)
}

read_data = function(path = "image_data/normal/", group_name = "normal"){
  
  filelist = list.files(path = path, pattern = "txt")
  filepath = file.path(path, filelist)
  
  fp = data.frame(
    filelist,
    filepath
  )
  
  data = vector()
  for(i in 1:dim(fp)[1]){
    f = read.delim(file = fp$filepath[i], header = FALSE)
    file_name = fp$filelist[i]
    names(f) = c("x", "y")
    f$index = 1:dim(f)[1]
    f$file_name = file_name
    f$condition = group_name
    data = rbind.data.frame(data, f)
    rm(f)
  }
  
  return(data)
  
}

data = rbind.data.frame(
  ### Load image data from normal group
  read_data(path = "image_data/normal/", group_name = "normal"),
  ### Load image data from diseased group 1
  read_data(path = "image_data/diseased/batch1/", group_name = "diseased"),
  ### Load image data from diseased group 2
  read_data(path = "image_data/diseased/batch2/", group_name = "diseased")
)


### Normalize the data set based on the weight-bearing position and scale
data_normalized = vector()
for(f in unique(data$file_name)){
  
  file_data = filter(data, file_name == f) %>%
    filter(!(index %in% c(1, 2, 3)))
  
  index6_position = filter(file_data, index == 6)[,1:2]
  index7_position = filter(file_data, index == 7)[,1:2]
  index5_position = filter(file_data, index == 5)[,1:2]
  
  distance67 = get_distance(index6_position, index7_position)
  distance56 = get_distance(index5_position, index6_position)
  
  n_point = max(file_data$index)
  wb_xy = filter(file_data, index == ifelse(n_point<=90, n_point,90))
  
  file_data$x = file_data$x - wb_xy$x
  file_data$y = file_data$y - wb_xy$y

  file_data$x = file_data$x/distance67
  file_data$y = -file_data$y/distance56
  
  data_normalized = rbind.data.frame(data_normalized,
                                     file_data)
}

data = data_normalized
rm(data_normalized)

save(data, file = "data.rda")







# ### Load image data from normal group
# filelist = list.files(path = "image_data/normal/", pattern = "txt")
# filepath = paste0("image_data/normal/", filelist)
# 
# fp = data.frame(
#   filelist = filelist, 
#   filepath = filepath
# )
# 
# normal_data = vector()
# for(i in 1:dim(fp)[1]){
#   f = read.delim(file = fp$filepath[i], header = F)
#   names(f) = c("x", "y")
#   file_name = fp$filelist[i]
#   f$index = 1:dim(f)[1]
#   f$file_name = file_name
#   f$condition = "normal"
#   normal_data = rbind.data.frame(normal_data, f)
#   rm(f)
# }
# 
# ### load image data from diseased group -- batch1
# filelist = list.files(path = "image_data/diseased/batch1/", pattern = "txt")
# filepath = paste0("image_data/diseased/batch1/", filelist)
# 
# fp_batch1 = data.frame(
#   filelist = filelist,
#   filepath = filepath
# )
# 
# ### load image data from diseased group -- batch2
# filelist = list.files(path = "image_data/diseased/batch2/", pattern = "txt")
# filepath = paste0("image_data/diseased/batch2/", filelist)
# 
# fp_batch2 = data.frame(
#   filelist = filelist,
#   filepath = filepath
# )
# 
# diseased_data = vector()
# 
# for(i in 1:dim(fp_batch1)[1]){
#   f = read.delim(file = fp_batch1$filepath[i], header = F)
#   names(f) = c("x", "y")
#   file_name = fp_batch1$filelist[i]
#   f$index = 1:dim(f)[1]
#   f$file_name = file_name
#   f$condition = "diseased1"
#   diseased_data = rbind.data.frame(diseased_data, f)
#   rm(f)
# }
# 
# for(i in 1:dim(fp_batch2)[1]){
#   f = read.delim(file = fp_batch2$filepath[i], header = F)
#   names(f) = c("x", "y")
#   file_name = fp_batch2$filelist[i]
#   f$index = 1:dim(f)[1]
#   f$file_name = file_name
#   f$condition = "diseased2"
#   diseased_data = rbind.data.frame(diseased_data, f)
#   rm(f)
# }
# 
# 
# ### Combine the normal and diseased data
# data = rbind.data.frame(normal_data, diseased_data) %>%
#   mutate(condition = ifelse(condition != "normal", "diseased", "normal"))
# 
# 
# data_normalized = vector()
# for(f in unique(data$file_name)){
#   
#   file_data = filter(data, file_name == f) %>%
#     filter(!(index %in% c(1, 2, 3)))
#   
#   index6_position = filter(file_data, index == 6)[,1:2]
#   index7_position = filter(file_data, index == 7)[,1:2]
#   index5_position = filter(file_data, index == 5)[,1:2]
#   
#   distance67 = get_distance(index6_position, index7_position)
#   distance56 = get_distance(index5_position, index6_position)
#   
#   n_point = max(file_data$index)
#   wb_xy = filter(file_data, index == ifelse(n_point<=90, n_point,90))
#   
#   file_data$x = file_data$x - wb_xy$x
#   file_data$y = file_data$y - wb_xy$y
#   
#   # file_data$x = file_data$x - index6_position$x
#   # file_data$y = file_data$y - index6_position$y
#   
#   file_data$x = file_data$x/distance67
#   file_data$y = -file_data$y/distance56
#   
#   
#   
#   data_normalized = rbind.data.frame(data_normalized,
#                                      file_data)
# }
# 
# data = data_normalized
# 
# save(data, file = "data.rda")



