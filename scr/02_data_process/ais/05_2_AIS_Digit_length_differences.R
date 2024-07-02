# ---------------------------------------------------------------------------
# 05 - AIS and satellite statistics differences analysis
# ----------------------------------------------------------------------

# 5.2) Differences of length between pairs of ships identified by AIS and satellite

#      Select pairs of vessel identified by methods

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)




# load scene info
df_gpkg <- st_read("data/output/scene_data.gpkg")
# extracts ids
ids <- unique(df_gpkg$img_ID)

# load AIS boat position interpolated and satellite vessels
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
sat <- read.csv("data/output/shipProc.csv", sep = ";")

# prepare ais data: filter ships with lenght == 0
ais <- ais %>% filter(length != 0)

# prepare satellite data
sat <- sat %>% 
  filter(duplicated == FALSE) %>%   
  filter(navigationStatus != "navigating") %>%  
  filter(lowCertainty == FALSE)   

# select variables of interest
sat <- sat %>% select(item_id, shipType, longitude, latitude, length_m)
ais <- ais %>% select(img_ID, type, longitude, latitude, length)

# add method field
sat$method <- "satellite"
ais$method <- "ais"

# stadanrized fields names between databases
sat <- sat %>%
  rename(length = length_m,
         type = shipType,
         img_ID = item_id)

# convert to sf object for further spatial analysis
sat <- st_as_sf(sat, coords = c("longitude", "latitude"), crs = st_crs(df_gpkg))
ais <- st_as_sf(ais, coords = c("longitude", "latitude"), crs = st_crs(df_gpkg))


# for each PlanetScope Scene (based in its img id)

# empty df for add pairs of ship
pairs <- data.frame(img_ID = character(),
                    ais_length = numeric(),
                    sat_length = numeric(),
                    ais_type = character(),
                    sat_type = character())

t <- Sys.time()

for (id in 1:length(ids)) {

  # Filter ais and satellite boat position by scene ID
  ais_s <- ais %>% filter(img_ID == id)
  sat_s <- sat %>% filter(img_ID == id)
  
  # add id to each vessel digitizalied into the different scenes
  # sat_s$shipID <- seq_along(sat_s$img_ID)
  
  # combine df 
  df <- rbind(ais_s, sat_s)
  
  # identify nearest ship
  nearest <- st_nearest_feature(df, parwise = TRUE)
  df$near_shipIndex <- st_nearest_feature(df, parwise = TRUE)
  # get distance (m) between centroid and its nearest centroid identified
  # equal distance pairs of nearest points
  df$dist <- st_distance(df, df[nearest,], by_element = TRUE)
  
  # extract ais position and its nearest satellite ship
  df_ais <- df %>% filter(method == "ais")
  # extract df index of nearst satellite based ship position
  ships_sat_index <- df_ais$near_shipIndex
  
  ## for each satellite-ais pair identified ---------------------------------
  
    for (index in ships_sat_index) {
      # extract satellite ship position by df index identified
      sat_ship <- df[index,]
      # extract ais pair by index identified
      ais_index <- sat_ship$near_shipIndex
      ais_ship <- df_ais[ais_index,]
      # df of pair position data
      pair_r <- data.frame(img_ID = ais_ship$img_ID,
                           ais_length = ais_ship$length,
                           sat_length = sat_ship$length,
                           ais_type = ais_ship$type,
                           sat_type = sat_ship$type)
      # append to df
      pairs <- rbind(pairs, pair_r)
    }
}

Sys.time() - t








# -----------------------------------------------------------------------------
# Statistica

# t-student???

# Differenece between number of vessel identified by method per scene
t_test <- t.test(pairs$ais_length, pairs$sat_length, paired = TRUE)
print(t_test)
# Note: t = -9.2938, df = 129, p-value = 4.802e-16










