library(here)
require(ggplot2)
require(reshape2)

if(exists('data_flat_terrain_1') == FALSE){
  filepath_flat_terrain_1 = here('data', 'flatTerrain_01-withGPS.csv')
  data_flat_terrain_1 = read.csv(filepath_flat_terrain_1, header=TRUE)
}

if(exists('filepath_flat_terrain_2') == FALSE){
  filepath_flat_terrain_2 = here('data', 'flatTerrain_02-withGPS.csv')
  data_flat_terrain_2 = read.csv(filepath_flat_terrain_2, header=TRUE)
}

if(exists('filepath_steep_terrain_1') == FALSE){
  filepath_steep_terrain_1 = here('data', 'steepTerrain_01-withGPS.csv')
  data_steep_terrain_1 = read.csv(filepath_steep_terrain_1, header=TRUE)
}

if(exists('filepath_steep_terrain_2') == FALSE){
  filepath_steep_terrain_2 = here('data', 'steepTerrain_02-withGPS.csv')
  data_steep_terrain_2 = read.csv(filepath_steep_terrain_2, header=TRUE)
}

if(exists('filepath_steep_terrain_3') == FALSE){
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

joints_suspension = list(
  Pan='Pan',
  InnerLeg='IL', 
  OuterLeg='OL')

joints_drive = list(
  WheelSteering='WS',
  WheelDrive='WD'
)

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


build_power_draw_df = function(df){
  row_count = nrow(df)
  
  power_draw_df = data.frame(
    'Locomotion' = numeric(row_count),
    'Suspension' = numeric(row_count),
    'Total' = numeric(row_count),
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


pwm_drive_node_names = get_drive_nodes('PWM')
current_drive_node_names = get_drive_nodes('current')

data_selected = data_list$STEEP_3

data_power_draw = build_power_draw_df(data_selected)

dev.new()
plot.ts(data_power_draw)

dev.new()
par(mar = c(5, 4, 4, 4) + 0.3)
plot(x=data_selected$odoPos_x,
     y=data_power_draw$Locomotion,
     xlab='Odometry Distance [m]',
     ylab='Power [W]',
     type='l', col='red')

lines(x=data_selected$odoPos_x, 
      y=data_power_draw$Drive,
      col='blue')

lines(x=data_selected$odoPos_x, 
      y=data_power_draw$Suspension,
      col='green')


# Allow a second plot on the same graph
par(new=TRUE)
plot(x=data_selected$odoPos_x, y=get_slope_angles(data_selected),
     xlab="", ylab="", ylim=c(0,30), 
     axes=FALSE, type='l', lwd=2, col="black")

mtext("Slope Angle [deg]", side=4) 
axis(4, ylim=c(0,30), las=1)

# Legend and title.
legend('topleft', legend=c('Locomotion', 'Drive', 'Suspension'),
       col=c('red', 'blue', 'green'), lty=1, cex=1)

plot_title = paste('Steep Power Upslope\n Wp = ', round(max(data_power_draw$Locomotion), 2), ' W', sep='')
title(main=plot_title)

# dev.new()
# data = data_selected[current_drive_node_names]
# plot.ts(data)
# 
# dev.new()
# data = data_selected[c('odoPos_x', 'odoPos_y', 'odoPos_z')]
# plot.ts(data)



