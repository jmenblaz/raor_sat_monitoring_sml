#------------------------------------------------------------------------------
# 03. Coverage satellite area and vessel metrics per satellite image
#------------------------------------------------------------------------------

# 1) Extract information for each image processed
# 2) Area digitalized, sample effort and Vessel metrics

library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)

library(sp)
library(units)
library(raster)




# ------------------------------------------------------------------------------
# 1) extract total, daily and weekly scene satellite coverage -------------------

# list images (folders) in dataset folder
images <- list.dirs("data/planet", recursive = FALSE)

dfr <- list()

# add bb of scenes into list
for (i in 1:length(images)) {
  # image ID and acquired info
  img <- images[i]
  imgID <- basename(img)
  # scene extension (km2)
  scenef <- list.files(img, pattern = paste0(imgID,".json"), recursive = TRUE, full.names = TRUE)
  scene  <- st_read(scenef)
  scene  <- scene[, c("id", "geometry")]
  # add result to list or append
  dfr[[i]] <- scene
}


# convert scene into raster format and stack
# Raster base based on bounding box area
# load study area
bb <- st_read('data/gis/study_area/study_area_raor.gpkg')

# extension of study area (bounding box)
ext <- st_bbox(bb)

# coordinates max min
y_max <- st_bbox(ext)["ymax"] 
y_min <- st_bbox(ext)["ymin"] 
x_max <- st_bbox(ext)["xmax"] 
x_min <- st_bbox(ext)["xmin"] 
# Resolution in degrees: 0.001 ≈ 111 m | 0.0005 ≈ 50 m
rbase <- raster(xmn = x_min, xmx= x_max, ymn= y_min, ymx= y_max,
                crs=CRS("+proj=longlat +datum=WGS84"),
                resolution=c(0.0001,0.0001), vals=NULL)

# create a empty list for rasters
rlist <- list()

for (i in 1:length(dfr)) {
  # extract scene
  p <- dfr[i]
  # change format: 1) list -> df -> sf
  p <- as.data.frame(p)
  p <- st_as_sf(p)
  # rasterize / rasterizar
  r <- rasterize(p, rbase)
  # NA values as 0 to keep the total bb extension
  r[is.na(r)] <- 0
  # add to list
  rlist[[i]] <- r
}

# stack rs
rstack <- stack(rlist)

### algebra map / raster calculate
# 1) total scenes covering bb
r <- sum(rstack)
# export/save raster
p <- "data/output/raster/01_num_scene_coverage.tif"
writeRaster(r, p, overwrite = TRUE)

# 2) number scene per week covering bb

# 25 Aug-20 Sept = 27 days
# 27 days = 3.86 weeks ≈ 4 weeks per year
# 8 years of study period -> from 2016 to 2023
# 32 weeks of scenes analyzed
# 224 days of time period analyzed

rw <- r / 32
# export/save raster
p <- "data/output/raster/02_num_week_scene_coverage.tif"
writeRaster(rw, p, overwrite = TRUE)

rd <- r / 224
# export/save raster
p <- "data/output/raster/03_num_day_scene_coverage.tif"
writeRaster(rd, p, overwrite = TRUE)












# ------------------------------------------------------------------------------
# 2) Area digitalized, sample effort and Vessel metrics ------------------------

# 2.1) Extract information for each image processed -----------------
# list images (folders) in dataset folder
images <- list.dirs("data/planet", recursive = FALSE)

# empty list
dfr <- list()

time <- Sys.time()
for (i in 1:length(images)) {
  # info
  print(paste("Processing image", i, "out of", length(images)))
  # image ID and acquired info
  img <- images[i]
  imgID <-  basename(img)
  acquired <- as.POSIXct(gsub(".*(\\d{8}_\\d{6}).*", "\\1", imgID), format="%Y%m%d_%H%M%S")
  
  # scene extension (km2)
  scenef <- list.files(img, pattern = paste0(imgID,".json"), recursive = TRUE, full.names = TRUE)
  scene  <- st_read(scenef)
  
  # extension and unit change m2 -> km2
  scene_ext <- set_units(st_area(scene), "km^2")
  
  # cloud extension in the scene (km2)
  cloudf <- list.files(img, pattern = "cloud.gpkg$", recursive = TRUE, full.names = TRUE)
  clouds <- st_read(cloudf)

  # obtain digitized area from the scene 
  # 1) No clouds
  if (nrow(clouds) == 0) {               
    clouds_ext       <- set_units(0, "km^2")
    digit_area       <- scene$geometry
    digit_area_ext   <- set_units(st_area(digit_area), "km^2")
  } else {
    # 2) Clouds presence
    if (nrow(clouds) > 1) {
      clouds         <- st_union(clouds) # union of all clouds patches
      clouds_area    <- st_intersection(clouds, scene) # clip clouds digitalized to scene bb
      clouds_ext     <- set_units(st_area(clouds_area), "km^2")
      digit_area     <- st_difference(scene, clouds)
      digit_area     <- st_union(digit_area)
      digit_area_ext <- set_units(st_area(digit_area), "km^2")
    }
    else { # (nrows clouds == 1) single clouds patch
      clouds_area    <- st_intersection(clouds, scene)
      clouds_ext     <- set_units(st_area(clouds_area), "km^2")
      digit_area     <- st_difference(scene, clouds)
      digit_area     <- st_union(digit_area)
      digit_area_ext <- set_units(st_area(digit_area), "km^2")
    }
  }
  # standardized format of digitized area in scene
  digit_area <- st_cast(digit_area, "MULTIPOLYGON")
  
  # import ship digitized data
  shipf <- list.files(img, pattern = "ship.gpkg$", recursive = TRUE, full.names = TRUE)
  # load data
  ships <- st_read(shipf)
  
  # spatial filter - ships in MPAnt and anchoring areas
  # ships <- st_intersection(ships, (MPAnt + buffer))
  
  # filter ships: avoid duplicated and navigation
  ships <- ships %>% 
    filter(duplicated == FALSE) %>%
    filter(navigationStatus == "anchor")
  # number of vessel
  nships <- nrow(ships)
  # number of vessel / km2 of digitized area
  nships_digitkm2 <- nships/digit_area_ext
  
  # summary result of image
  img_data <- data.frame(
    img_ID = imgID,
    acquired_UTC  = acquired,
    acquired_local = scene$datetime, 
    instrument = scene$instruments,
    scene_area_km2 = scene_ext,
    clouds_area_km2 = clouds_ext,
    digitized_area_km2 = digit_area_ext,
    number_ships = nships,
    number_ships_digit_km2 = nships_digitkm2,
    geometry = digit_area
  )
  
  # add result to list or append
  dfr[[i]] <- img_data
  
}
Sys.time() - time

# combine dataframe list into a single datafreme
scenedf <- rbindlist(dfr)

# % cloud coverage over total area scene
scenedf$cloud_cov_percentage <- (scenedf$clouds_area_km2 / scenedf$scene_area_km2) * 100
scenedf$cloud_cov_percentage <- as.numeric(scenedf$cloud_cov_percentage)

# % area digitalized over total per scene (or difference between cloud and scene area)
# scenedf$digit_area_cov_percentage <- (100 - scenedf$cloud_cov_percentage)
scenedf$digit_area_cov_percentage <- (scenedf$digitized_area_km2 / scenedf$scene_area_km2) * 100
scenedf$digit_area_cov_percentage <- as.numeric(scenedf$digit_area_cov_percentage)

# calculate % cloud cover
scenedf$digitized_cover <- (scenedf$digitized_area_km2 / scenedf$scene_area_km2) * 100 
scenedf$cloudcover <- 100 - scenedf$digitized_cover 

# export scene results .csv and .gpkg
p <- "data/output/scene_data.csv"
# df without geometries
# remove geom / geomtry from sf object
scenedf_nogeo <- scenedf %>% st_drop_geometry()
write.csv(scenedf_nogeo, p, row.names = FALSE)
# gpkg with geometries
st_write(scenedf, "data/output/scene_data.gpkg", append = FALSE)


# 2.2) Summary results  --------------------------------------------------------
# Total area (mean of km2 of digit_area) 
# days analyzed (% respect of 224 total days in the period of days)










# 2.3) Obtain digitalized effort -----------------------------------------------
# total area digitalized, NO cloud coverage

# 2.3.1) total area digitalized in number of scenes without cloud coverage --
# Note: rbase object created previously

# dfr <- read_sf("data/output/scene_data.gpkg")

rlist <- list()

for (i in 1:length(dfr)) {
  # extract scene
  p <- dfr[i]
  # change format: 1) list -> df -> sf
  p <- as.data.frame(p)
  p <- st_as_sf(p)
  # rasterize
  r <- rasterize(p, rbase)
  # NA values as 0
  r[is.na(r)] <- 0
  # append
  rlist[[i]] <- r
}

# stack rs
rstack <- stack(rlist)
# total scenes covering bb
r <- sum(rstack)
# export/save raster
p <- "data/output/raster/04_digit_area_num_scene.tif"
writeRaster(r, p, overwrite = TRUE)

# total scenes covering bb per week
rw <- r / 32
# export/save raster
p <- "data/output/raster/05_digit_area_num_scene_week.tif"
writeRaster(rw, p, overwrite = TRUE)



# 2.3.2) vessel density in total area digitalized -----------------------------
# Note: rbase object created previosly

rlist <- list()

for (i in 1:length(dfr)) {
  # extract scene
  p <- dfr[i]
  # change format: 1) list -> df -> sf
  p <- as.data.frame(p)
    # extract vessel density value
    shpd <- as.numeric(p$number_ships_digit_km2)
  # continue change format  
  p <- st_as_sf(p)
  # rasterize
  r <- rasterize(p, rbase)
  # raster value 1 as vessel density and Na as 0
  r[r == 1]   <- shpd
  r[is.na(r)] <- 0
  # append
  rlist[[i]] <- r
}

# stack rs
rstack <- stack(rlist)
# total vessel density
r <- sum(rstack)
# export/save raster
p <- "data/output/raster/06_ship_density_sum_digit_area.tif"
writeRaster(r, p, overwrite = TRUE)



# mean ship density (num_ship/km2) in digitalized area by number scenes
# ship density / number of scene digitalized
rs <- raster("data/output/raster/04_digit_area_num_scene.tif")
rd <- raster("data/output/raster/06_ship_density_sum_digit_area.tif")
# mean
r <- rd/rs



# 2.3.3) % Cloud of cloud cover -----------------------------------------------
# Note: rbase object created previosly

# list folders imgs
images <- list.dirs("data/planet", recursive = FALSE)
# list scene data extracted previously
data <- read.csv("data/output/scene_data.csv")

rlist <- list()

t <- Sys.time()
for (i in 1:length(images)) {
  # info
  print(paste("Processing image", i, "out of", length(images)))
  # image ID and acquired info
  img <- images[i]
  imgID <-  basename(img)
  
  # cloudcover of the image
  data_img <- data %>% filter(img_ID == imgID)
  cloudcover <- (data_img$cloudcover)
  
  # load cloud digitalized
  cloudf <- list.files(img, pattern = "cloud.gpkg$", recursive = TRUE, full.names = TRUE)
  clouds <- st_read(cloudf)

  if (nrow(clouds) == 0) {
    r <- rbase
    # NA values as 0
    r[is.na(r)] <- 0
    # append
    rlist[[i]] <- r
  }
  else {
    # rasterize and add values
    r <- rasterize(clouds, rbase)
    # values > 0 -> cloud cover
    r[r > 0] <- cloudcover
    # NA values as 0
    r[is.na(r)] <- 0
    # append
    rlist[[i]] <- r
  }
}
Sys.time() - t  # 1:30 min


# stack rs
rstack <- stack(rlist)
# mean cloudcover into scenes analyzed
r <- mean(rstack)
# plot(r)
p <- "data/output/raster/07_mean_cloudcover_scenes.tif"
writeRaster(r, p, overwrite = TRUE)


















