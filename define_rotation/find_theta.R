library(dplyr)
library(ggplot2)
library(ggpubr)
library(sp)
library(spData)
library(spdep)

find_theta = function(pointset_filename,
                      left_bottom_index = 7,
                      left_up_index = 4,
                      right_bottom_index = 6,
                      insertion_peak_index = 12,
                      tilt_marker_index = 13,
                      scale_index_range = c(1,3),
                      marker_index_range = c(4,14),
                      superior_index_range = c(15,36),
                      insertion_index_range = c(37,90)){
  cat(paste0("processing ", pointset_filename, "...\n"))
  try({
    scale_index_start = scale_index_range[1]
    scale_index_end = scale_index_range[2]
    marker_index_start = marker_index_range[1]
    marker_index_end = marker_index_range[2]
    superior_index_start = superior_index_range[1]
    superior_index_end = superior_index_range[2]
    insertion_index_start = insertion_index_range[1]
    insertion_index_end = insertion_index_range[2]
    
    d = read.delim(pointset_filename, header = FALSE)
    names(d) = c("x", "y")
    
    scale_factor_x = ((d$x[right_bottom_index] - d$x[left_bottom_index])^2 +
                        (d$y[right_bottom_index] - d$y[left_bottom_index])^2)^0.5
    scale_factor_y = ((d$x[left_up_index] - d$x[left_bottom_index])^2 +
                        (d$y[left_up_index] - d$y[left_bottom_index])^2)^0.5
    
    d$index = 1:dim(d)[1]
    d$anno = NA
    d$anno[scale_index_start:scale_index_end] = "scale"
    d$anno[marker_index_start:marker_index_end] = "marker"
    d$anno[superior_index_start:superior_index_end] = "superior"
    d$anno[insertion_index_start:insertion_index_end] = "insertion"
    
    d$y = -d$y                 # Flip the image upside down
    wb_x = d$x[length(d$x)]    # Find weightbearing X coord
    wb_y = d$y[length(d$y)]    # Find weightbearing Y coord
    d$x = d$x - wb_x           # Zero at weightbearing point
    d$y = d$y - wb_y           # Zero at weightbearing point
    d$x = d$x/scale_factor_x   # Standardize X scale
    d$y = d$y/scale_factor_y   # Standardize Y scale
    
    # Define insertion pizza center
    insertion_peak_x = d$x[d$index==insertion_peak_index] # Pizza center point
    insertion_peak_y = d$y[d$index==insertion_peak_index] # Pizza center point
    
    d$x = d$x - insertion_peak_x # Zero at pizza center point
    d$y = d$y - insertion_peak_y # Zero at pizza center point
    
    # Define box
    left_bottom_x = d$x[d$index==left_bottom_index]
    left_bottom_y = d$y[d$index==left_bottom_index]
    left_up_x = d$x[d$index==left_up_index]
    left_up_y = d$y[d$index==left_up_index]
    right_bottom_x = d$x[d$index==right_bottom_index]
    right_bottom_y = d$y[d$index==right_bottom_index]
    
    # Use pre-fit scaling variables
    x_o_scale = 0.412
    y_o_scale = 0.454
    rd_scale = 0.417
    
    # Define the standard circle
    x_length = right_bottom_x-left_bottom_x
    y_length = left_up_y - left_bottom_y
    o_x = x_length*x_o_scale + left_bottom_x
    o_y = y_length*y_o_scale + left_bottom_y
    r = (x_length^2 + y_length^2)^0.5*rd_scale
    
    # Plot specific circle
    gg_circle = function(r, xc, yc, color="black", fill=NA, ...) {
      x = xc + r*cos(seq(0, pi, length.out=100))
      ymax = yc + r*sin(seq(0, pi, length.out=100))
      ymin = yc + r*sin(seq(0, -pi, length.out=100))
      annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
    }
    
    plt_circle = ggplot(data = filter(d, anno != "scale"),
                        aes(x = x, y = y, index = index, anno = anno)) +
      geom_point(aes(fill = anno), shape = 21, color = "black") +
      gg_circle(r=r, xc=o_x, yc=o_y) + 
      labs(fill = "") +
      xlab("X") +
      ylab("Y") +
      theme_pubr() +
      theme(text = element_text(size = 15))
    
    # Get standard circle based on y
    get_standard_circle_x = function(
    y, o_x, o_y, r
    ){
      return((r^2-(y-o_y)^2)^0.5 + o_x)
    }
    
    # Define standard circle curve
    standard_circle_x = sapply(d$y[d$anno=="insertion"],
                               get_standard_circle_x, o_x, o_y, r)
    original_curve_x = d$x[d$anno=="insertion"]
    original_curve_y = d$y[d$anno=="insertion"]
    
    # Transform curves to the original axis
    standard_circle_x_transformed = standard_circle_x * scale_factor_x
    original_curve_x_transformed = original_curve_x * scale_factor_x
    original_curve_y_transformed = original_curve_y * scale_factor_y
    
    # Construct transformation data frame
    original_curve_transformed = data.frame(x = original_curve_x_transformed,
                                            y = original_curve_y_transformed)
    standard_circle_transformed = data.frame(x = standard_circle_x_transformed,
                                             y = original_curve_y_transformed)
    
    # Calculate original loss
    original_loss = mean(abs(standard_circle_transformed$x - original_curve_transformed$x))
    
    # Determining the best rotational angle
    rotational_loss_vector = vector()
    rotation_angles = seq(0, pi/6, 0.005)
    for(rotation_angle in rotation_angles){
      post_rotation_xy = Rotation(original_curve_transformed, 
                                  rotation_angle)
      post_rotation_xy = as.data.frame(post_rotation_xy)
      names(post_rotation_xy) = c("x","y")
      
      # Calculate rotational loss
      rotational_loss = abs(post_rotation_xy$x - standard_circle_transformed$x)
      rotational_loss = mean(rotational_loss)
      rotational_loss_vector = c(rotational_loss_vector, rotational_loss)
    }
    rm(post_rotation_xy)
    
    # Compiling loss and obtain optimum angle
    loss_df = data.frame(angle = rotation_angles, 
                         loss = rotational_loss_vector)
    loss_df$angle = loss_df$angle * 180 / pi
    optimum_insertion_angle = loss_df$angle[which.min(loss_df$loss)[1]]
    
    # Plot loss
    plt_loss = ggplot(data = loss_df, aes(x = angle, y = loss)) + 
      geom_point() + 
      geom_line() + 
      xlab("Insertion Angle (Degree)") + 
      ylab("Rotational Loss") +
      ggtitle(label = NULL, 
              subtitle = paste0("Optimum Insertion Angle: ", 
                                round(optimum_insertion_angle,1),
                                " Degrees")) + 
      theme_pubr() +
      theme(text = element_text(size = 15))
    
    # Plot simulated rotation
    optimum_insertion_radian = optimum_insertion_angle*pi/180
    post_rotation_xy = Rotation(original_curve_transformed, 
                                optimum_insertion_radian)
    post_rotation_xy = as.data.frame(post_rotation_xy)
    names(post_rotation_xy) = c("x","y")
    
    post_rotation_xy$index = -1
    post_rotation_xy$anno = "Optimum Rotation"
    post_rotation_xy$x = post_rotation_xy$x / scale_factor_x
    post_rotation_xy$y = post_rotation_xy$y / scale_factor_y
    post_rotation_xy = rbind.data.frame(d, post_rotation_xy)
    plt_rotated = ggplot(data = filter(post_rotation_xy, anno != "scale"),
                         aes(x = x, y = y, index = index, anno = anno)) +
      geom_point(aes(fill = anno), shape = 21, color = "black") +
      gg_circle(r=r, xc=o_x, yc=o_y) + 
      labs(fill = "") +
      xlab("X") +
      ylab("Y") +
      theme_pubr() +
      theme(text = element_text(size = 15))
    
    # Calculate pitch angle change
    tilt_marker_x = d$x[d$index==tilt_marker_index]*scale_factor_x # Tilt marker point
    tilt_marker_y = d$y[d$index==tilt_marker_index]*scale_factor_y # Tilt marker point
    rotated_wb_x = tail(post_rotation_xy,1)$x*scale_factor_x
    rotated_wb_y = tail(post_rotation_xy,1)$y*scale_factor_y
    wb_x = tail(d,1)$x*scale_factor_x
    wb_y = tail(d,1)$y*scale_factor_y
    
    original_pitch_angle = -tanh(
      (tilt_marker_y-wb_y)/(tilt_marker_x-wb_x)
    ) * 180 / pi
    
    rotated_pitch_angle = -tanh(
      (tilt_marker_y-rotated_wb_y)/(tilt_marker_x-rotated_wb_x)
    ) * 180 / pi
    
    delta_pitch_angle = rotated_pitch_angle - original_pitch_angle
    
    # Miscellaneous measurements
    # Ratio1: direct length between 10-14/direct length between 8-9
    # Ratio2: length between 8-9/length between 4-7
    # curve length ratio between 36-90/14-90
    
    # NOTE: some of the points are not given direct anatomical meanings
    
    p10_x = d$x[d$index == 10]*scale_factor_x
    p10_y = d$y[d$index == 10]*scale_factor_y
    p14_x = d$x[d$index == 14]*scale_factor_x
    p14_y = d$y[d$index == 14]*scale_factor_y
    p8_x = d$x[d$index == 8]*scale_factor_x
    p8_y = d$y[d$index == 8]*scale_factor_y
    p9_x = d$x[d$index == 9]*scale_factor_x
    p9_y = d$y[d$index == 9]*scale_factor_y
    p4_x = d$x[d$index == 4]*scale_factor_x
    p4_y = d$y[d$index == 4]*scale_factor_y
    p7_x = d$x[d$index == 7]*scale_factor_x
    p7_y = d$y[d$index == 7]*scale_factor_y
    
    d10_14 = ((p10_x - p14_x)^2 + (p10_y - p14_y)^2)^0.5
    d8_9 = ((p8_x - p9_x)^2 + (p8_y - p9_y)^2)^0.5
    d4_7 = ((p4_x - p7_x)^2 + (p4_y - p7_y)^2)^0.5
    ratio1 = d10_14/d8_9
    ratio2 = d8_9/d4_7
    
    # insertion_length
    insertion = filter(d, anno == "insertion")
    insertion_length = sum((diff(insertion$x)^2 + diff(insertion$y)^2)^0.5)
    superior = filter(d, anno == "superior")
    superior_length = sum((diff(superior$x)^2 + diff(superior$y)^2)^0.5)
    curve_length_ratio = insertion_length/(insertion_length + superior_length)
    
    # Compile result list
    result_list = list(
      `Scaling Factor` = data.frame(x = scale_factor_x, y = scale_factor_y),
      `Scaled Data` = d,
      `Standard Circle` = plt_circle,
      `Loss` = loss_df,
      `Loss Plot` = plt_loss,
      `Optimum Insertion Angle` = optimum_insertion_angle,
      `Simulated Rotation` = plt_rotated,
      `Original Pitch Angle` = original_pitch_angle,
      `Rotated Pitch Angle` = rotated_pitch_angle,
      `Delta Pitch Angle` = delta_pitch_angle,
      `Ratio 10_14/8_9` = ratio1,
      `Ratio 8_9/4_7` = ratio2,
      `Curve Length Ratio` = curve_length_ratio
    )
    
    return(result_list)
  })
  
  result_list = list(
    `Optimum Insertion Angle` = NA,
    `Original Pitch Angle` = NA,
    `Rotated Pitch Angle` = NA,
    `Delta Pitch Angle` = NA,
    `Ratio 10_14/8_9` = NA,
    `Ratio 8_9/4_7` = NA,
    `Curve Length Ratio` = NA
  )
  
  return(result_list)
  
}

theta = find_theta(pointset_filename = "Diseased_combined/1272902.txt", insertion_peak_index = 90); theta

# theta = find_theta(pointset_filename = "image_data/diseased/batch1/1272902.txt", insertion_peak_index = 90)

# save(theta, file = "example_theta.rda")
