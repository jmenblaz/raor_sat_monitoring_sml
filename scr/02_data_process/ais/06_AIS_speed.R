

# Leveraging earth observation data to monitor boat-based recreational fishing 

# ---------------------------------------------------------------------------
# 06 - AIS and satellite statistics differences analysis
# ----------------------------------------------------------------------

# 1) Compare AIS speed (from interpolated position) to navigation status digitalized

# 2) Compare speed between interpolated position between before and after opening fishing ban



library(dplyr)
library(lubridate)



#---------------------------------------------------------------------------------------
# 1) Compare AIS speed (from interpolated position) to navigation status digitalized

# load AIS-Sat pairs data

pairs <- read.csv("data/output/ais/length_ais_sat_pairs.csv")

# filter neighbors with >= 50m distance
pairs$dist <- as.numeric(pairs$dist)
pairs <- pairs %>% filter(dist <= 45)
pairs <- pairs %>% filter(ais_length < 200) # one outlier for 500 in AIS data


# use a 3 knots (double of the speed for filter AIS as navigating) 
# 1 knots = 1.852 km/h = 0.514


# descriptive summary of data
summary_stats <- pairs %>%
  group_by(sat_navigationStatus) %>%
  summarise(
    mean_speed = mean(ais_speed, na.rm = TRUE),
    median_speed = median(ais_speed, na.rm = TRUE),
    min_speed = min(ais_speed, na.rm = TRUE),
    max_speed = max(ais_speed, na.rm = TRUE),
    sd_speed = sd(ais_speed, na.rm = TRUE),
    count = n()
  )
print(summary_stats)




pairs_nav <- pairs %>% filter(sat_navigationStatus == "navigating")
nrow(pairs_nav)
pairs_anch <- pairs %>% filter(sat_navigationStatus == "anchor")
nrow(pairs_anch)

# Calculate the number of vessel classify as navigating with a AIS speed interpolate
# > 1.54 m/s or 3 knots
count <- sum(pairs_nav$ais_speed > 1.028)
result <- (count*100)/nrow(pairs_nav)
result

# Calculate the number of vessel classify as anchored with a AIS speed interpolate
# < 1.54 m/s or 3 knots
count <- sum(pairs_anch$ais_speed < 1.028)
result <- (count*100)/nrow(pairs_anch)
result

ggplot(pairs, aes(x = sat_navigationStatus, y = ais_speed)) +
  geom_boxplot() +
  labs(title = "Velocidad por Estado de Navegación",
       x = "Estado de Navegación",
       y = "Velocidad AIS (m/s)") +
  theme_minimal()





# -------------------------------------------------------------------------------
# 2) Compare speed between interpolated position between before and after opening fishing ban

# load AIS boat position interpolated and satellite vessels
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# convert to sf
ais <- st_as_sf(ais, coords = c("longitude", "latitude"))
fa <- read_sf("data/gis/fa/fishing_area.gpkg")

# add CRS of fa
st_crs(ais) <- st_crs(fa)

plot(fa$geom)
plot(ais$geometry, add = TRUE)

# filer interpolated position inside fishing area
ais <- st_intersection(ais, fa)
plot(ais$geometry, add = TRUE)



# convert date
ais <- ais %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))

# Filter dates before September
ais_b <- ais %>%
  filter(month(timestamp) < 9)

# results
summary(ais_b$speed)
sd(ais_b$speed)
summary(ais_a$speed)
sd(ais_a$speed)

wilcox.test(ais_b$speed, ais_a$speed)


