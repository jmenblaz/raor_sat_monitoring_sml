#------------------------------------------------------------------------------
# 06. AIS scene results
#------------------------------------------------------------------------------

# 1) % of vessel with AIS and digitalized

# 2) number of vessel with AIS per scene

# 3) ship lenght (eslora) analysis
#  2.1 - number of ais ship with length >= 10m and 15m
#  2.2 - 


library(dplyr)
library(sf)
library(data.table)


# load data ---------------------------------------------------------
# AIS scene
ais <- read.csv("data/output/ais_scenes_interpolated.csv")

# scene digitalized data 
df_gpkg <- st_read("data/output/scene_data.gpkg")

# ship digitized
ships <- read.csv("data/output/shipProc.csv")
ships <- ships %>% filter(duplicated == "FALSE") # filter ships duplicated



# 1) % of vessel with AIS and digitized --------------------------------------
n_ais <- nrow(ais)  # 229 ->  num ship with AIS
n_ships <- nrow(ships)  # 4446 -> ship digitized (NO duplicated - anchoring and navigating)

(n_ais/n_ships)*100  # % -> ** 5.44% of total ship presented AIS  **






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
ais <- ais %>% filter(length != 0)  # filter ships with lenght == 0

#   3.1)
n_ais <- ais |> filter(ais$length >= 10)
(nrow(n_ais)/nrow(ais))*100   # 89.67 % (217) of ship with AIS has >= 10m  

n_ais <- ais |> filter(ais$length >= 15)
(nrow(n_ais)/nrow(ais))*100   # 45.45 % (110) of tota ship with AIS >= 15m

n_ships <- ships |> filter(length_m >= 15)
(nrow(n_ships)/nrow(ships))*100  # 61.74 % (2745) of digitized ship >= 15m

#   3.2) mean and sd of length
summary(ais$length)
summary(ships$length_m)



