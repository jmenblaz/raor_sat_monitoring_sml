# ---------------------------------------------------------------------------
# 05 - AIS and satellite statistics differences analysis
# ----------------------------------------------------------------------

# 5.2) Differencse of length between pairs of ships identified by AIS and satellite



library(dplyr)
library(tidyr)
library(ggplot2)

ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# filter ships with lenght == 0
ais <- ais %>% filter(length != 0)

# ship digitized
ships <- read.csv("data/output/shipProc.csv", sep = ";")
ships <- ships %>% filter(duplicated == "FALSE") # filter ships duplicated

# values min-max lengt
sat_length <- range(ships$length_m)
ais_length <- range(ais$length)

# extract info and combien df
ais <- ais %>% select(img_ID, length)
ships <- ships %>% 
  select(item_id, length_m) %>%
  rename(img_ID = item_id,
         length = length_m)

ais$method = "ais"
ships$method = "sat"

df <- rbind(ais,ships)
df$length <- as.numeric(df$length)
df$method <- as.factor(df$method)

# ------------------------------------------------------------------------------
# 5.1) Statistics differences in total results obtanied

# Prepare data for analysis
df <- df %>% group_by(img_ID, method) %>%
  summarize(n = n(),
            length = median(length))

df_wide <- df %>%
  pivot_wider(names_from = method, values_from = c(n, length))
# Note: t-student for more than 2 methods. For >2 use ANOVA

# using only image with data for both method...
df_wide <- na.omit(df_wide) 

# Welch’s t-test (var.equal in fuction ->)
t_test_length <- t.test(df_wide$length_ais, df_wide$length_sat, paired = TRUE, var.equal = FALSE)
print(t_test_length)


# Differenece between number of vessel identified by method per scene
t_test_length <- t.test(df_wide$n_ais, df_wide$n_sat, paired = TRUE)
print(t_test_length)
# Note: t = -9.2938, df = 129, p-value = 4.802e-16



# Potential plot
# df from differences
length_diff <- data.frame(
  difference = df_wide$length_ais - df_wide$length_sat
)


# Plot
ggplot(length_diff, aes(x = difference)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribución de las Diferencias en la Longitud de los Barcos",
    x = "Diferencia en la Longitud (AIS - SAT)",
    y = "Frecuencia"
  ) +
  theme_minimal()





# ------------------------------------------------------------------------------
# 5.2) Difference of length between pairs of ships -----------------------------
#      identified by AIS and satellite              ----------------------------


# Select pairs of vessel identified by methods

# Load AIS boat position interpolated and satellite vessels
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
sat <- read.csv("data/output/shipProc.csv", sep = ";")

# load scene info

# ids <-  

# prepare satellite data for fishing effort analysis
sat <- sat %>% 
  filter(duplicated == FALSE) %>%   
  filter(navigationStatus != "navigating") %>%  
  filter(lowCertainty == FALSE)   


# Filter by data by scene ID

# get centroid of digitalization
cent <- st_centroid(ships)

# identify nearest ship centroid
nearest <- st_nearest_feature(cent)
cent$near_shipID <- st_nearest_feature(cent)

# obtain data of lengeth in the pair 
# df 
#'MMSI
#'ID centroid
#'leng AIS 
#'lengt digit
#'differenc...
#'

# get distance (m) between centroid and its nearest centroid identified 
cent$dist <- st_distance(cent, cent[nearest,], by_element = TRUE)











# ------------------------------------------------------------------------------
# 2) Spatial analysis                              -----------------------------
#     2.1) intersect (matches) 
#     2.2) distance between centroids
# combine files, calculate distance?
# remove duplicate distances (par of centroids)

# list of images
folders <- list.dirs("data/gis/cross_validation/", recursive = F)


# result list
result <- list()

# counter
counter <- 1

for (folder in folders) {
  
  # empty list to add digitized ships in the image by different people
  ships <- list()
  
  # get ship files
  shipFiles <- list.files(folder, pattern = "_ship_", recursive = TRUE, full.names = TRUE)
  
  # read ship files and append to list
  for (f in shipFiles) {
    ship_file <- st_read(f)
    
    #if (is.null(ship_file)) {
    #  next  # if no vessels found, skip to next file
    #} else {
    # add file to list
    st_crs(ship_file) <- st_crs(st_read(shipFiles[1]))
    ships <- c(ships, list(ship_file))
  }
  
  # image id
  image_id <- basename(folder)
  
  # Union ship of different files into same sf objetct
  ships <- do.call(rbind, ships)
  # add number id to ships
  ships$shipID <- seq_along(ships$imageID)
  
  ###  2.1) intersect (matches)
  intersections <- st_intersects(ships)
  
  # number of intersection of each vessel by digitizer
  intersection_df <- data.frame(
    imageID <- ships$imageID,
    shipID = ships$shipID,
    digitizer = ships$digitizer,
    num_intersections = sapply(intersections, 
                               function(x) length(x)))
  
  # % of match of each vessel between digitizers
  n_digitizer <- n_distinct(ships$digitizer)  # digitizers num
  intersection_df$percent_match_dgt <- ((intersection_df$num_intersections * 100) / n_digitizer)
  
  
  ###  2.2) distance centroids
  cent <- st_centroid(ships)
  # identify nearest ship centroid
  nearest <- st_nearest_feature(cent)
  cent$near_shipID <- st_nearest_feature(cent)
  # get distance (m) between centroid and its nearest centroid identified 
  cent$dist <- st_distance(cent, cent[nearest,], by_element = TRUE)
  
  
  # combine results
  df_result <- merge(intersection_df, cent[, c("shipID", "near_shipID", "dist")], by = "shipID", all.x = T)
  
  # add to list
  result[[counter]] <- df_result
  
  
  # counter + 1
  counter <- counter + 1
  
}

# result list to df
result <- do.call(rbind, result)
# rename column
names(result)[names(result) == "imageID....ships.imageID"] <- "imageID"
# import / save with spatial data
st_write(result, "data/output/cross_validation/result/result_ships_cross_validation.gpkg", append = F)
# remove geometries of centroids, remove column
result$geometry <- NULL
# export / save result
write.csv(result, "data/output/cross_validation/result/result_ships_cross_validation.csv")
# made summary of result
summary(result)














# re-run summary
result <- read.csv("data/output/cross_validation/result/result_ships_cross_validation.csv")

# summary by image
resume <- result |>
  group_by(imageID) |>
  summarize(
    percent_match_dgt = mean(percent_match_dgt),
    dist_mean = mean(dist),
    dist_median = median(dist)
  )


# Number ships digitzed by digitizer
# digitizer 1: 191
# digitizer 2: 186

median(df$percent_match_dgt)
# 97.48% of match between vessel digitalized









