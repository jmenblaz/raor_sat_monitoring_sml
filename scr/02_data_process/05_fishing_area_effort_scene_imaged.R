# --------------------------------------------------
# 05- fishing effort - process
# --------------------------------------------------

# Calculate fishing effort by area (km2) of fishing area digititalized or imaged
# use previous results obtained in 05_fishing_area_effort.R

# 1) Obtained fishing area imaged by each PlanetScope scene digitalized
# 2) transform the fishing effort results to this area

# ----------------------------------------------------------

library(sp)
library(sf)
library(dplyr)
library(raster)

# load spatial data
mpa  <- read_sf("data/gis/mpa/mpapb.gpkg")
# load fishing area
fa <- st_read("data/gis/fa/fishing_area.gpkg")
# assign same CRS
st_crs(fa) <- st_crs(mpa)
rm(mpa) 

# study area
sa <- st_read("data/gis/study_area/study_area_raor.gpkg")
st_crs(sa) <- st_crs(fa)



# 1) Obtained fishing area imaged by each PlanetScope scene digitalized
# - Get union of the digitalized area between differenrt Planetscope scens per day
# - iimaged % of area per scene and per day...

# 2) transform the fishing effort results to this area

# Use scene data calculated before in order to add information about fa digitalized

# Calculate the fishing area digitalized per day (using the different Planetscope scenes)


# read Planetscope scene data
scenes <- st_read("data/output/scene_data.gpkg")

# format date for further analysis
scenes$date <- format(scenes$acquired_local, "%Y-%m-%d")




# 1) Calculate the fishing area digitalized per scene (and its %)
# Not that digitalized area (cloud free) have been calculated previously (scene_data.csv)

scenes$digitized_fa_area_km2 <- NA
scenes$digitized_fa_cover <- NA

t <- Sys.time()

for (i in 1:nrow(scenes)) {
  # info
  print(paste("Processing image", i, "out of", nrow(scenes)))
  
  # extract data for each scene (row)
  scene <- scenes[i,]
  
  plot(scene$geom)
  plot(fa$geom, add = TRUE)
  
  # intersect fishing area with digitalized area
  fa_imaged <- st_intersection(scene, fa)
  # plot(fa_imaged$geom, fill = "skyblue3")  # check

  
  if (nrow(fa_imaged) > 0) { # intersection with fishing area
    # area km2
    area_fa <- as.numeric(st_area(fa)) / 1e6
    area_fa_imaged <- as.numeric(st_area(fa_imaged)) / 1e6
    
    # digitized area
    scenes$digitized_fa_area_km2[i] <- area_fa_imaged
    # % of fa cover digitized
    scenes$digitized_fa_cover[i] <- (area_fa_imaged / area_fa) * 100
    
  # No intersection with fishing area  
  } else { 
    scenes$digitized_fa_area_km2[i] <- 0
    scenes$digitized_fa_cover[i] <- 0
  }
}
  

Sys.time() - t # 2 min

# update file with the new information
st_write(scenes, "data/output/scene_data.gpkg", append = FALSE)









# --------------------------------------------------------------------------------
# 2) Calculate fa digitized / imaged by day in order to obtain a fishing effort / km2 of imaged fa
# add info of fa area digitalized to AIS and Satellite fishing efforts dataset

ais_f <- read.csv("data/output/fishing_effort/ais_fishing_effort_data.csv")
sat_f <- read.csv("data/output/fishing_effort/satellite_fishing_effort_data.csv")


# create empty fields for fill
sat_f$number_scenes <- NA
ais_f$number_scenes <- NA
# boat densities per km2 of imaged fa
sat_f$ship_dens_km2_fa_imaged <- NA
ais_f$ship_dens_km2_fa_imaged <- NA
# percenage of fa cover imaged 
sat_f$digitized_fa_cover <- NA
ais_f$digitized_fa_cover <- NA

fa_total_area_km2 <- as.numeric(st_area(fa)) / 1e6


# create empty field for study area coverage
sat_f$digitized_sa_cover <- NA
ais_f$digitized_sa_cover <- NA

sa_total_area_km2 <- as.numeric(st_area(sa)) / 1e6



# for each day of study period

for (i in 1:nrow(sat_f)) {
  print(paste("Processing day of fishing effort:", i, "out of", nrow(sat_f)))
  
  day <- sat_f$timestamp[i]
  scenes_day <- scenes %>% filter(date == day)
  
  if (nrow(scenes_day) == 0) {
    sat_f$number_scenes[i] <- 0
    ais_f$number_scenes[i] <- 0
    sat_f$ship_dens_km2_fa_imaged[i] <- 0
    ais_f$ship_dens_km2_fa_imaged[i] <- 0
    sat_f$digitized_fa_cover[i] <- 0
    ais_f$digitized_fa_cover[i] <- 0
    sat_f$digitized_sa_cover[i] <- 0
    ais_f$digitized_sa_cover[i] <- 0
    next
  }
  
  if (nrow(scenes_day) > 1) {
    
    union_geom <- scenes_day %>%
      st_make_valid() %>%
      st_union()
    union_geom <- st_make_valid(union_geom)
    
    fa_imaged_geom <- st_intersection(union_geom, fa)
    fa_imaged_area <- as.numeric(st_area(fa_imaged_geom)) / 1e6
    
    sa_imaged_geom <- st_intersection(union_geom, sa)
    sa_imaged_area <- as.numeric(st_area(sa_imaged_geom)) / 1e6
    
  } else {    # only 1 scene
    # intersect digitized area - study area 
    sa_imaged_geom <- st_intersection(scenes_day, sa)
    sa_imaged_area <- as.numeric(st_area(sa_imaged_geom)) / 1e6
    # fishing area calculate previously
    fa_imaged_area <- scenes_day$digitized_fa_area_km2
  }
  
  sat_f$number_scenes[i] <- nrow(scenes_day)
  ais_f$number_scenes[i] <- nrow(scenes_day)
  
  # % de cobertura del FA
  fa_cover_percent <- (fa_imaged_area / fa_total_area_km2) * 100
  fa_cover_percent <- min(fa_cover_percent, 100)
  sat_f$digitized_fa_cover[i] <- fa_cover_percent
  ais_f$digitized_fa_cover[i] <- fa_cover_percent
  
  # % de cobertura del SA
  sa_cover_percent <- (sa_imaged_area / sa_total_area_km2) * 100
  sa_cover_percent <- min(sa_cover_percent, 100)
  sat_f$digitized_sa_cover[i] <- sa_cover_percent
  ais_f$digitized_sa_cover[i] <- sa_cover_percent
  
  if (fa_imaged_area == 0) {
    sat_f$ship_dens_km2_fa_imaged[i] <- 0
    ais_f$ship_dens_km2_fa_imaged[i] <- 0
  } else {
    sat_f$ship_dens_km2_fa_imaged[i] <- sat_f$ships[i] / fa_imaged_area
    ais_f$ship_dens_km2_fa_imaged[i] <- ais_f$ships[i] / fa_imaged_area
  }
}

# Clean final dataframe
# values > 99.99 == 100% 
sat_f$digitized_fa_cover[sat_f$digitized_fa_cover > 99.99] <- 100
sat_f$digitized_sa_cover[sat_f$digitized_sa_cover > 99.99] <- 100

ais_f$digitized_fa_cover[ais_f$digitized_fa_cover > 99.99] <- 100
ais_f$digitized_sa_cover[ais_f$digitized_sa_cover > 99.99] <- 100


# update files with the new information
write.csv(ais_f, "data/output/fishing_effort/ais_fishing_effort_data.csv", row.names = FALSE)
write.csv(sat_f, "data/output/fishing_effort/satellite_fishing_effort_data.csv", row.names = FALSE)



