# ---------------------------------------------------------------------------
# 05 - AIS and satellite statistics differences analysis
# ----------------------------------------------------------------------

# 5.2) Differences of length between pairs of ships identified by AIS and satellite
 
#      1) Select pairs of vessel identified by methods
#      2) Statistical analysis of differences (t-test)



library(sf)
library(dplyr)
library(tidyr)

# for testing
# id <- ids[210]
#id <- "20200909_101620_0f28"



# load scene info
df_gpkg <- st_read("data/output/scene_data.gpkg")


# load AIS boat position interpolated and satellite vessels
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
sat <- read.csv("data/output/shipProc.csv", sep = ";")

# extract scene IDs with AIS data
ids <- unique(ais$img_ID)
# prepare ais data: filter ships with lenght == 0
ais <- ais %>% filter(length != 0)

# prepare satellite data
sat <- sat %>%
  # Note: not filter duplicateds due we work by scene
  # filter(duplicated == FALSE) %>%   
  filter(navigationStatus != "navigating") %>%  
  filter(lowCertainty == FALSE)   

# select variables of interest
sat <- sat %>% select(item_id, shipType, longitude, latitude, length_m)
ais <- ais %>% select(img_ID, type, longitude, latitude, length)

# add method field
sat$method <- "satellite"
ais$method <- "ais"

# standardized fields names between databases
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

for (id in ids) {

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

  ## for each satellite-ais pair identified ---------------------------------
    for (r in 1:nrow(df_ais)) {
      # filter by each AIS boat
      ais_ship <- df_ais[r,]
      # extract nearest boat
      index <- ais_ship$near_shipIndex
      # extract nearest boat position (and info) by df index identified
      sat_ship <- df[index,]
      # df of pair position data
      pair_r <- data.frame(img_ID = ais_ship$img_ID,
                           ais_length = ais_ship$length,
                           sat_length = sat_ship$length,
                           ais_type = ais_ship$type,
                           sat_type = sat_ship$type,
                           # for check that the pairs are correctly made 
                           # between AIS-Satellite
                           ais_method = ais_ship$method,
                           sat_method = sat_ship$method)
      # append to empty df of results
      pairs <- rbind(pairs, pair_r)
    }
}

Sys.time() - t # 1 min aprox.

# save pairs results

#' There are some scenes in which 
#' 1- The nearest boat to AIS position it other AIS position
#' 2- Scenes with AIS interpolated position but not Satellite digitization
#'    only one case: scene ID: 20200909_101620_0f28

# AIS - AIS cases
pairs_ais_ais <- pairs %>% filter(ais_method == "ais" & sat_method == "ais")
# AIS - Satellite cases
pairs <- pairs %>% filter(ais_method == "ais" & sat_method == "satellite")

# export / save csv
write.csv(pairs,"data/output/ais/length_ais_sat_pairs.csv", row.names = FALSE)
# export / save csv
write.csv(pairs_ais_ais,"data/output/ais/length_ais_ais_pairs.csv", row.names = FALSE)




# -----------------------------------------------------------------------------
# Statistics

# t-student???

# Differences between number of vessel identified by method per scene
t_test <- t.test(pairs$ais_length, pairs$sat_length, paired = TRUE)
print(t_test)
# Note: t = -1.2485, df = 369, p-value = 0.2126










