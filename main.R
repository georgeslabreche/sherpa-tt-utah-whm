library(here)

filepath_flat_terrain_1 = here('data', 'flatTerrain_01-withGPS.csv')
filepath_flat_terrain_2 = here('data', 'flatTerrain_02-withGPS.csv')
filepath_steep_terrain_1 = here('data', 'steepTerrain_01-withGPS.csv')
filepath_steep_terrain_2 = here('data', 'steepTerrain_02-withGPS.csv')
filepath_steep_terrain_3 = here('data', 'steepTerrain_03_heavySlip-withGPS.csv')

data_flat_terrain_1 = read.csv(filepath_flat_terrain_1, header=TRUE)
data_flat_terrain_2 = read.csv(filepath_flat_terrain_2, header=TRUE)
data_steep_terrain_1 = read.csv(filepath_steep_terrain_1, header=TRUE)
data_steep_terrain_2 = read.csv(filepath_steep_terrain_2, header=TRUE)
data_steep_terrain_3 = read.csv(filepath_steep_terrain_3, header=TRUE)