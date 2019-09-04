library(here)
require(ggplot2)
require(reshape2)

if(exists('data_flat_terrain_1') == FALSE){
  filepath_flat_terrain_1 = here('data', 'flatTerrain_01-withGPS.csv')
  data_flat_terrain_1 = read.csv(filepath_flat_terrain_1, header=TRUE)
}

if(exists('data_flat_terrain_2') == FALSE){
  filepath_flat_terrain_2 = here('data', 'flatTerrain_02-withGPS.csv')
  data_flat_terrain_2 = read.csv(filepath_flat_terrain_2, header=TRUE)
}

if(exists('data_steep_terrain_1') == FALSE){
  filepath_steep_terrain_1 = here('data', 'steepTerrain_01-withGPS.csv')
  data_steep_terrain_1 = read.csv(filepath_steep_terrain_1, header=TRUE)
}

if(exists('data_steep_terrain_2') == FALSE){
  filepath_steep_terrain_2 = here('data', 'steepTerrain_02-withGPS.csv')
  data_steep_terrain_2 = read.csv(filepath_steep_terrain_2, header=TRUE)
}

if(exists('data_steep_terrain_3') == FALSE){
  filepath_steep_terrain_3 = here('data', 'steepTerrain_03_heavySlip-withGPS.csv')
  data_steep_terrain_3 = read.csv(filepath_steep_terrain_3, header=TRUE)
}

data_list = list(
  FLAT_1 = data_flat_terrain_1,
  FLAT_2 = data_flat_terrain_2,
  STEEP_1 = data_steep_terrain_1,
  STEEP_2 = data_steep_terrain_2,
  STEEP_3 = data_steep_terrain_3
)

# This function gets the slop angles values associated with each measurement.
# Slope angle values are based on Table 5: Slope angles for Steep-Slope tests
# from Cordes et al. 2018.
get_slope_angles = function(df){

  slope_angles = c()
  
  for(odoPos_x in df$odoPos_x){
    
    if(odoPos_x <= 1){
      slope_angles = c(slope_angles, 9.5)
      
    }else if(odoPos_x <= 3){
      slope_angles = c(slope_angles, 10)
      
    }else if(odoPos_x <= 4){
      slope_angles = c(slope_angles, 11)
      
    }else if(odoPos_x <= 5){
      slope_angles = c(slope_angles, 15)
      
    }else if(odoPos_x <= 6){
      slope_angles = c(slope_angles, 16)
      
    }else if(odoPos_x <= 7){
      slope_angles = c(slope_angles, 28)
      
    }else if(odoPos_x <= 8){
      slope_angles = c(slope_angles, 22)
      
    }else if(odoPos_x <= 9){
      slope_angles = c(slope_angles, 25)
      
    }else if(odoPos_x <= 11){
      slope_angles = c(slope_angles, 28)
      
    }else if(odoPos_x <= 13){
      slope_angles = c(slope_angles, 20)
      
    }else if(odoPos_x <= 14){
      slope_angles = c(slope_angles, 15)
      
    }else if(odoPos_x <= 16){
      slope_angles = c(slope_angles, 10)
      
    }else{
      slope_angles = c(slope_angles, 0)
    }    
  }
  
  return(slope_angles)
}

# List of suspension motor column names.
joints_suspension = list(
  Pan='Pan',
  InnerLeg='IL', # Inner Leg
  OuterLeg='OL') # Outer Let

# List of drive motor column names.
joints_drive = list(
  WheelSteering='WS', # Wheel Steering
  WheelDrive='WD' # Wheel Driving.
)

# All motor column names.
joints_locomotion = c(joints_suspension, joints_drive)

legs = list(
  FrontLeft='fl',
  FrontRight='fr',
  RearLeft='rl',
  RearRight='rr')


get_propulsion_nodes = function(measurement, joints){
  nodes = c()
  for(joint in joints){
    for(leg in legs){
      node_id = paste(measurement, '_', joint, '_', leg, sep='')
      nodes = c(nodes, node_id)
    }
  }
  
  return(nodes)  
}

get_suspension_nodes = function(measurement){
  get_propulsion_nodes(measurement, joints_suspension)
}

get_drive_nodes = function(measurement){
  get_propulsion_nodes(measurement, joints_drive)
}

get_locomotion_nodes = function(measurement){
  get_propulsion_nodes(measurement, joints_locomotion)
}


get_leg_propulsion_nodes = function(measurement, leg, joints){
  nodes = c()
  for(joint in joints){
    node_id = paste(measurement, '_', joint, '_', leg, sep='')
    nodes = c(nodes, node_id)
  }
  return(nodes)
}

get_leg_suspension_nodes = function(measurement, leg){
  get_leg_propulsion_nodes(measurement, leg, joints_suspension)
}

get_leg_drive_nodes = function(measurement, leg){
  get_leg_propulsion_nodes(measurement, leg, joints_drive)
}

get_leg_locomotion_nodes = function(measurement, leg){
  get_leg_propulsion_nodes(measurement, leg, joints_locomotion)
}

get_leg_power_nodes = function(leg_location){
  get_leg_locomotion_nodes('PWM', leg_location)
}

get_leg_current_nodes = function(leg_location){
  get_leg_locomotion_nodes('current', leg_location)
}

# This function build the dataframe containing all required power data.
build_power_draw_df = function(df){
  row_count = nrow(df)
  
  power_draw_df = data.frame(
    'Locomotion' = numeric(row_count),
    'Suspension' = numeric(row_count),
    'Drive' = numeric(row_count),
    'Pan' = numeric(row_count),
    'IL' = numeric(row_count),
    'OL' = numeric(row_count),
    'WS' = numeric(row_count),
    'WD' = numeric(row_count))
  
  for(joint in joints_locomotion){
    for(leg in legs){
      pwd_col_name = paste('PWM_', joint, '_', leg, sep='')
      current_col_name = paste('current_', joint, '_', leg, sep='')
      
      power_draw_df[joint] = power_draw_df[joint] + (48 * abs(df[pwd_col_name]) * abs(df[current_col_name]))
    }
  }
  
  power_draw_df$Drive = power_draw_df$WS + power_draw_df$WD
  power_draw_df$Suspension = power_draw_df$Pan + power_draw_df$IL + power_draw_df$OL
  power_draw_df$Locomotion = power_draw_df$Drive + power_draw_df$Suspension
  
  return(power_draw_df)
}

# Get the local minima, maxima, and media of a given vector of values.
# This function is a modified verion of the one found here: https://stackoverflow.com/a/43061365
# The modification consists of including media.
inflect = function(x, threshold = 1){
  up   = sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down = sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    = cbind(x,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]),
       maxima = which(apply(a, 1, max) == a[,1]),
       media = which(apply(a, 1, median) == a[,1]))
}


#pwm_drive_node_names = get_drive_nodes('PWM')
#current_drive_node_names = get_drive_nodes('current')

data_selected = data_list$STEEP_3
data_power_draw = build_power_draw_df(data_selected)

#############################
# Plot all power draw data. #
#############################
dev.new()
plot.ts(data_power_draw, col='blue')

#######################################################
# Plot local minima, maxima, and media of power draw. #
#######################################################
plot_maxima_and_minima = function(x, y, threshold, title=''){
  data_indices = inflect(y, threshold=threshold)
  local_maxima_indices = data_indices$maxima
  local_minima_indices = data_indices$minima
  local_media_indices = data_indices$media
  
  plot(x=x,
       y=y,
       xlab='Odometry Distance [m]',
       ylab='Power [W]',
       type='l', col='grey')
  
  lines(x=x[local_minima_indices], 
        y=y[local_minima_indices],
        col='coral2')
  
  lines(x=x[local_maxima_indices],
        y=y[local_maxima_indices],
        col='red')
  
  lines(x=x[local_media_indices], 
        y=y[local_media_indices],
        col='black', lty=2)
  
  legend('topleft', legend=c('Measured', 'Local Minima', 'Local Maxima', 'Media'),
         col=c('grey', 'coral2', 'red', 'black'), lty=c(1, 1, 1, 2), cex=0.8)
  
  title(main=title)
}

# Locomotion.
dev.new()
plot_maxima_and_minima(x=data_selected$odoPos_x, y=data_power_draw$Locomotion, threshold=100, 'Locomotion Power Draw')

# Drive.
dev.new()
plot_maxima_and_minima(x=data_selected$odoPos_x, y=data_power_draw$Drive, threshold=100, 'Drive Power Draw')

# Suspension.
dev.new()
plot_maxima_and_minima(x=data_selected$odoPos_x, y=data_power_draw$Suspension, threshold=100, 'Suspension Power Draw')


###########################################################
# Plot suspension, drive, and locomotion power draw data. #
# Include slope angle.                                    #
###########################################################
dev.new()

# We want to a plot with 2 x-axes (power draw and slope angle).
par(mar = c(5, 4, 4, 4) + 0.3)

# Plot locomotion power draw.
plot(x=data_selected$odoPos_x,
     y=data_power_draw$Locomotion,
     xlab='Odometry Distance [m]',
     ylab='Power [W]',
     ylim=c(0,370),
     type='l', col='red')

# Plot drive power draw.
lines(x=data_selected$odoPos_x,
      y=data_power_draw$Drive,
      col='blue')

# Plot Suspension power draw.
lines(x=data_selected$odoPos_x,
      y=data_power_draw$Suspension,
      col='green')


# Allow a second plot on the same graph.
# This is for the second x-axis (slope angle).
par(new=TRUE)
plot(x=data_selected$odoPos_x, y=get_slope_angles(data_selected),
     xlab="", ylab="", ylim=c(0,30),
     axes=FALSE, type='l', lwd=2, col="black")

mtext("Slope Angle [deg]", side=4, padj=3.5)
axis(4, ylim=c(0,30), las=1)

# Legend and title.
legend('topleft',
       title='Power Draw',
       legend=c('Locomotion', 'Drive', 'Suspension'),
       col=c('red', 'blue', 'green'), lty=1, cex=0.8)

legend('topright', legend=c('Slope Angle'),
       col=c('black'), lty=1, cex=0.8, lwd=2)

# Plot title.
plot_title = paste('Steep Power Upslope\n Wp = ', round(max(data_power_draw$Locomotion), 2), ' W', sep='')
title(main=plot_title)

# dev.new()
# data = data_selected[current_drive_node_names]
# plot.ts(data)
# 
# dev.new()
# data = data_selected[c('odoPos_x', 'odoPos_y', 'odoPos_z')]
# plot.ts(data)



