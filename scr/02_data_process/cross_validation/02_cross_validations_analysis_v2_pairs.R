

#------------------------------------------------------------------------------
# 02 - Cross-validation analysis between digitalizers
#------------------------------------------------------------------------------

# Check the differences between the different digitalizers involved
# using the results of the same five images


# 0) Digitization results for script 01_cross_validations_analysis.R

# For this version: differences of length 
# between pairs of ships identified by differenrt analyst.


# Version 2: Using pairs of ships identfied by differenrt analyst (distance to neares ship =< 50)
#      1) Select pairs of vessel identified by methods (nearest AIS and satellite records)
#      2) Statistical analysis of differences (t-test)

# Same method used to compare AIS and Satellite and AIS records



library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)



# load cross-validation digitalization result¡ (see 01_cross_validations_analysis.R script)
data <- read.csv('data/output/cross_validation/shipProc.csv')
df_gpkg <- st_read("data/output/scene_data.gpkg")

# extract scene IDs with AIS data
ids <- unique(data$item_id)

unique(data$digitizer)

# convert to sf object for further spatial analysis
data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = st_crs(df_gpkg))

# empty df for add pairs of ship
pairs <- data.frame(img_ID = character(),
                    ais_length = numeric(),
                    sat_length = numeric(),
                    ais_type = character(),
                    sat_type = character())

t <- Sys.time()
# for each scene
for (id in ids) {

  # Filter digitalizations records by analist and scene ID
  df <- data %>% filter(item_id == id)
  
  # add id to each vessel digitizalied into the different scenes
  # sat_s$shipID <- seq_along(sat_s$img_ID)
  
  # identify nearest ship
  nearest <- st_nearest_feature(df, parwise = TRUE)
  df$near_shipIndex <- st_nearest_feature(df, parwise = TRUE)
  
  # get distance (m) between centroid and its nearest centroid identified
  # equal distance pairs of nearest points
  df$dist <- st_distance(df, df[nearest,], by_element = TRUE)
  
  records_digit1 <- sum(df$digitizer == (unique(df$digitizer)[1]))
  records_digit2 <- sum(df$digitizer == (unique(df$digitizer)[2]))
  
  # in some case that digitizer have been recorded differenrt number of ships,
  # use the lower number of record to crated a reliably pairs measures:
  
  if (records_digit1 > records_digit2) {
    df_digitizer1 <- df %>% filter(digitizer == (unique(df$digitizer))[2])
  } else {
    df_digitizer1 <- df %>% filter(digitizer == (unique(df$digitizer))[1])
  }

  
  ## for each digitizer1-digitizer2 pair identified ---------------------------------
    for (r in 1:nrow(df_digitizer1)) {
      # filter by each AIS boat
      dig1_ship <- df_digitizer1[r,]
      # extract nearest boat
      index <- dig1_ship$near_shipIndex
      # extract nearest boat position (and info) by df index identified
      dig2_ship <- df[index,]
      # df of pair position data
      pair_r <- data.frame(item_id = dig1_ship$item_id,
                           dig1_length = dig1_ship$length_m,
                           dig2_length = dig2_ship$length_m,
                           dig1_area = dig1_ship$ship_area,
                           dig2_area = dig2_ship$ship_area,
                           dig1_type = dig1_ship$shipType,
                           dig2_type = dig2_ship$shipType,
                           dist = dig1_ship$dist,
                           # for check that the pairs are correctly made 
                           # between digitizer1-digitizer2
                           digitizer1 = dig1_ship$digitizer,
                           digitizer2 = dig2_ship$digitizer)
      # append to empty df of results
      pairs <- rbind(pairs, pair_r)
    }
}

Sys.time() -  # 6 sec

# save pairs results
# export / save csv
write.csv(pairs,"data/output/cross_validation/length_dig1_dig2_pairs.csv", row.names = FALSE)




# -----------------------------------------------------------------------------
# Statistics

pairs <- read.csv("data/output/cross_validation/length_dig1_dig2_pairs.csv")
# 184

# filter neighbors with >= 50m distance
pairs$dist <- as.numeric(pairs$dist)
# pairs <- pairs %>% filter(dist <= 10)
# pairs <- pairs %>% filter(digitizer1 != digitizer2)

str(pairs)



# Calcular el RMSE entre las longitudes estimadas por los dos digitalizadores
rmse <- sqrt(mean((pairs$dig1_length - pairs$dig2_length)^2))

cat("RMSE:", rmse, "\n")

pearson_correlation <- cor(pairs$dig1_length, pairs$dig2_length)

# Mostrar el resultado
print(rmse)
print(pearson_correlation)



# t-student
# Differences between number of vessel identified by method per scene
t_test <- t.test(pairs$dig1_length, pairs$dig2_length, paired = TRUE)
print(t_test)

summary(pairs$ais_length)
sd(pairs$ais_length)
summary(pairs$sat_length)
sd(pairs$sat_length)
# Results:   t = -1.3174, df = 284, p-value = 0.1888


# Pearson correlation
pearson_corr <- cor(pairs$dig1_length, pairs$dig2_length, method = "pearson")
print(pearson_corr)

ggplot(pairs, aes(x = dig1_length, y = dig2_length)) +
  geom_point() +
  labs(title = "Scatter Plot: boat lenghts between digitizers",
       x = "dig1_length",
       y = "dig2_length") +
  ylim(0, 50) +
  theme_minimal() +
  stat_smooth(method = "lm", col = "red") +  # Añadir la línea de tendencia
  annotate("text", x = max(pairs$sat_length), y = 90, label = paste("Pearson: ", round(pearson_corr, 2)), hjust = 1)


