#------------------------------------------------------------------------------
# 06. AIS scene results
#------------------------------------------------------------------------------

# 1) % of vessel with AIS and digitized

# 2) number of vessel with AIS per scene

# 3) ship lenght (eslora) analysis
#  3.1 - number of ais ship with length >= 10m and 15m

# 4) % of ship categories from AIS data that have been detected




library(data.table)
library(dplyr)
library(sf)


# load data ---------------------------------------------------------
# AIS scene
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# scene digitalized data 
df_gpkg <- st_read("data/output/scene_data.gpkg")

# ship digitized
ships <- read.csv("data/output/shipProc.csv", sep = ";")
ships <- ships %>% filter(duplicated == "FALSE") # filter ships duplicated


# 1) % of vessel with AIS and digitized --------------------------------------
n_ais <- nrow(ais)  # 386 ->  num of unique ships with AIS
n_ships <- nrow(ships)  # 4442 -> ship digitized (NO duplicated - anchoring and navigating)

(n_ais/n_ships)*100  # % -> ** 8.69% of total ship found by satellite presented AIS  **



# 2) Number of ship with AIS per scene -----------------------------------------
# extract scenes ID
scenes <- unique(df_gpkg$img_ID)
ais_scene <- list()

# extract number of AIS per scene
for (s in 1:length(scenes)) {
  scene <- scenes[s]
  # filter ship in the scene
  scene_ais <- ais %>% filter (img_ID == scene)
  ais_ship <- nrow(scene_ais)
  
  # same coordinates, same position -> NO interpolation
  sr <- data.frame(img_ID = scene,
                   number_ships_ais = ais_ship)
  # add to list
  ais_scene[[s]] <- sr
  
} 

ais_scene <- rbindlist(ais_scene)

# combine with scenes dataframe info (.gpkg)
cmb <- left_join(df_gpkg, ais_scene, by = "img_ID")

#calculate % of vessel with ais of total of vessel digitalized per scene
cmb$percentage_ais_ships_digit <- (cmb$number_ships_ais/cmb$number_ships)*100
  
# export
st_write(cmb, "data/output/scene_ais_data.gpkg", append = FALSE)





# 3) ship length (eslora) analysis ---------------------------------------------

# filter length
ais <- ais %>% filter(length != 0)  # filter ships with length == 0 
# 15 vessels without length information -> 2.63% of the ships with AIS hadn't length information

#   3.1) length information
n_ais <- ais |> filter(ais$length >= 10)
(nrow(n_ais)/nrow(ais))*100   # 96.76 % (359) of ship with AIS has >= 10m  

n_ais <- ais |> filter(ais$length >= 15)
(nrow(n_ais)/nrow(ais))*100   # 51.48 % (191) of tota ship with AIS >= 15m

n_ships <- ships |> filter(length_m >= 15)
(nrow(n_ships)/nrow(ships))*100  # 61.70 % (2741) of digitized ship >= 15m

#   3.2) mean and sd of length
summary(ais$length) # mean = 20.72 +- 37.15
sd(ais$length)
summary(ships$length_m) # mean = 18.69 +- 8.84
sd(ships$length_m)


# 4) % of ship categories from AIS data that have been detected

# for reclassify categories (March et al., 2021, Nature communications)
class <- ais %>%
  distinct(mmsi, .keep_all = TRUE) %>%
  group_by(type) %>%
  count()

# unique ships detected with AIS
ships <- length(unique(ais$mmsi))
# percentage over total unique ships detected with AIS
class$percentage_tot_ships <- (class$n/ships)*100
print(class)
#   type             n         percentage_tot_shi
# 1 Fishing          2                 1.42
# 2 Other            2                 1.42
# 3 Passenger        9                 6.38
# 4 Recreational   128                 90.8 

# for raw AIS class
class <- ais %>%
  distinct(mmsi, .keep_all = TRUE) %>%
  group_by(type_name) %>%
  count()





