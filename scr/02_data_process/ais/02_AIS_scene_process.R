#------------------------------------------------------------------------------
# 04. Difference between vessel density AIS and satellite data
#------------------------------------------------------------------------------


# note: if only there is one position of the vessel 
# or the vessel didn't move between position -> NO interpolation (use original position)

# But: if NO interpolation and use the original position but the timestamp is 
# that acquired time -> AIS position discarted
# There is not certain that this vessel was at the sometime of satellite


# AIS: All AIS data per each day in +-0.05 degrees of study area or bounding box

# Workflow
# 1) filtrar AIS: +- 20 min scene timeacquired
# 2) interpolate trayectorias de todos los barcos (min or second) test...
# 3) filtrar aquellos registros que están dentro de la cobertura de la scenena
#    en el momento exacto del la fotografía
#  3.1) filtrar por el timestamp de la scene
#  3.2) clip, select or intersect by scene area


library(sf)
library(dplyr)
library(move)
library(data.table)

library(foreach)
library(doParallel)

# load data -------------------------------------------------------------------

# AIS (previously filtered for bbox of study area based on days of the unique timestamps scenes)
ais <- read.csv("data/ais/aisRaw.csv")
# remove column grt and dwt
ais <- subset(ais, select = -c(grt, dwt))
# rename columns lat and lon for move object
ais <- ais %>% 
  rename(latitude = lat,
         longitude = lon)

# scene info and spatial data
df_gpkg <- st_read("data/output/scene_data.gpkg")
# bbox study area
study_area <- st_read("data/gis/study_area/study_area_raor.gpkg")

# change timestamp format
ais$timestamp <- as.POSIXct(ais$timestamp)



# Filter AIS data and interpolate position at scene acquired timestamp --------

# cluster
cores <- detectCores() * 0.9
cl <- makeCluster(cores)
registerDoParallel(cl)


# parallel loop
t <- Sys.time()

ais_result <- rbindlist(foreach(l = 1:nrow(df_gpkg), .packages = c("sf", "dplyr", "move", "moveVis", "data.table", "lubridate"))  %dopar% {

  # extract scene
  scene <- df_gpkg[l, ]
  # print(paste("imageID: ", scene$img_ID))
  # timestamp
  tstamp <- scene$acquired_UTC
  # scene extent
  ext <- st_bbox(scene)
  # filter AIS dataset in the study area by scene timestamp (+- 20 min)
  ais_scene <- ais |> filter(
    timestamp >= (tstamp-(20*60)) &
      timestamp <= (tstamp+(20*60)))
  
  # remove duplicated AIS messages
  ais_scene <- ais_scene %>%
    distinct(timestamp, mmsi, .keep_all = TRUE) %>%
    arrange(mmsi, timestamp)
  
  # if AIS in the timestamp range
  if (nrow(ais_scene) == 0) {
    NULL  # == next in sequential loop
  }
  
  else {
    # convert to MoveStack object (there are different "animals")
    # Note: columns "latitude", "longitude" and "timestamp" are necessary
    m_stack <- move::move(x = ais_scene$longitude, 
                          y = ais_scene$latitude, 
                          time = ais_scene$timestamp, 
                          animal = ais_scene$mmsi,
                          proj = CRS("+proj=longlat +ellps=WGS84"),
                          data = ais_scene)
    
    # split into different moves objects per "animal ID"
    m_list <- split(m_stack)
    
    # per move object (i.e., per ship...) ------
    
    ship_scene <- list()
    
    for (s in 1:length(m_list)) {
      # extract move object
      m <- m_list[[s]]
      
      # extract coordinates from move object
      coords <- coordinates(m)
      
      # check if all position have same coordinates
      if(all(coords[-1,] == coords[1,])) {
        
        # same coordinates, same position -> NO interpolation
        # convert into dataframe and select first position
        p <- as(m, "data.frame")
        p <- p[1,]
        
        # same coordinates, same position -> NO interpolation
        sr <- data.frame(img_ID = scene$img_ID,
                         mmsi      = p$mmsi,
                         timestamp = p$timestamps,
                         longitude = p$coords.x1,
                         latitude  = p$coords.x2,
                         sensor    = p$sensor,
                         ship_name = p$ship_name,
                         ship_type = p$ship_type,
                         type_name = p$type_name,
                         type      = p$type,
                         ais_type_summary = p$ais_type_summary,
                         flag      = p$flag,
                         length    = p$length,
                         width     = p$width,
                         status    = p$status,
                         speed     = p$speed,
                         imo       = p$imo,
                         n         = p$n)
        
        # add to list
        ship_scene[[s]] <- sr
        
      } 
      else {
        # Different position -> Interpolation
        
        # interpolation by each second
        # ais_interp <- interpolateTime(m, time=as.difftime(1, units= "secs"), spaceMethod='euclidean')
        
        # check if min of timstamps of move object is higher than the scene timestamp
        # T == NO Interpolation
        # F == Interpolation
        if (tstamp < min(m@timestamps) || tstamp > max(m@timestamps))
          next
        
        # interpolation by unique scene timestamp
        ts <- as.POSIXct(tstamp,  format="%Y-%m-%d %H:%M:%S")
        ais_interp <- interpolateTime(m, time=ts, spaceMethod='euclidean')
        
        # convert positions into dataframe
        p <- as(ais_interp, "data.frame")
        
        # create df with interpolated position and ship features
        sr <- data.frame(img_ID = scene$img_ID,
                         mmsi      = p$mmsi,
                         timestamp = p$timestamps,
                         longitude = p$coords.x1,
                         latitude  = p$coords.x2,
                         sensor    = p$sensor,
                         ship_name = p$ship_name,
                         ship_type = p$ship_type,
                         type_name = p$type_name,
                         type      = p$type,
                         ais_type_summary = p$ais_type_summary,
                         flag      = p$flag,
                         length    = p$length,
                         width     = p$width,
                         status    = p$status,
                         speed     = p$speed,
                         imo       = p$imo,
                         n         = p$n)
        
        # add to list
        ship_scene[[s]] <- sr
      }
      
    }
    
    # for all the ships in the scene, combine into df by scene
    ship_scene <- rbindlist(ship_scene)
    
    # filter ships using acquired time scene
    # remove ships with >= acquired time scene
    # a) note: select only ships (or position) before the scene capture
    # ship_scene <- ship_scene |> filter(timestamp <= tstamp)
    
    # b) note: select only ship position at the same acquire time 
    #       OR ships with no movement or interpolation present before the scene capture
    ship_scene <- ship_scene |> filter(timestamp == tstamp | sensor == "original_position",)
    
    # filter ships using scene cover
    # convert to st object
    ship_scene <- st_as_sf(ship_scene, coords = c("longitude", "latitude"), crs = st_crs(scene))
    
    # select geometries from scene to avoid add other fields
    scene_geom <- scene["geom"]
    # intersection
    ship_scene <- st_intersection(scene_geom, ship_scene)
    
    # convert from sf to df to rbinlist paralell
    ship_scene <- as.data.frame(ship_scene)
    
    # filter scenes results without AIS ship position 
    if (nrow(ship_scene) == 0) {
      NULL  # == next in sequencial loop
    } 
    else {
      ship_scene
    }

    }
  
}, fill = TRUE)


Sys.time() - t  # ≈ 1.30 min
stopCluster()

# process result ------

# parse coordinates into final df
# extract latitude and coordinates from geom(column)
ais_result$geom <- as.character(ais_result$geom)
ais_result <- ais_result[, c("longitude", "latitude") := tstrsplit(gsub("[c()]", "", geom), ",")][, c("longitude", "latitude") := lapply(.SD, as.numeric), .SDcols = c("longitude", "latitude")]

# remove geom column
ais_result <- subset(ais_result, select = -geom)

# clean potenial duplicateds
ais_result <- unique(ais_result)

# save / export
write.csv(ais_result, "data/output/ais_scenes_interpolated.csv")
