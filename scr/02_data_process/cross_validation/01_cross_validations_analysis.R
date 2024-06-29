

#------------------------------------------------------------------------------
# 06. Cross-validation analysis between digitalizers
#------------------------------------------------------------------------------

# Check the differences between the different digitalizers involved
# using the results of the same five images

# 0) Digitization results*
# 1) Number of total vessel per images
#   1.1) Number of vessel navigation and vessel anchored
#   1.2) Length of vessel digitalizers
# 2) Spatial intersect (matches) and distance between centroids


# Note*: use 01_digitalization_procss.R script to extract digitalization data
#        functions: splitPolygon
#                   longest_side_of_polygon

# changes: add calculate of area of each ship
#          change ship files pattern  route


library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(tidyverse)

# 0) Digitization results -----------------------------------------------------

# functions
# splitPolygon
# longest_side_of_polygon

# input dir
indir <- "data/gis/cross_validation"
# output dir
outdir <- "data/output/cross_validation/"

# 0.1 Batch processing
shipFiles <- list.files(indir, pattern = "_ship_", recursive = TRUE, full.names = TRUE)

# data list [create empties list()]
data_list <- list()
nodata_list <- list()

# process data for each of the "_ship.gpkg" files

for(i in 1:length(shipFiles)){
  # info message
  print(paste("Processing file", i, "out of", length(shipFiles)))
  # get file
  ifile <- shipFiles[i]
  #filename <- str_split(ifile, "/", simplify = TRUE)[,2] # doesn´t work, at least in Windows
  filename <- basename(ifile)
  # import ship data
  ships <- st_read(ifile)
  if(nrow(ships)==0){
    nodata_list[[i]] <- data.frame(file = filename, noData = TRUE)
    next  # if no vessels found, next
  } 
  
  # calculate centroids and length
  for(j in 1:nrow(ships)){
    
    # select individual ship
    pol <- ships[j,]
    
    # get centroid of the ship
    cent <- st_centroid(pol)
    jcoords <- st_coordinates(cent)
    ships$longitude[j] <- jcoords[1]
    ships$latitude[j] <- jcoords[2]
    
    # measure length of the ships
    ll <- longest_side_of_polygon(pol)
    jlength <- st_length(ll) %>% as.numeric()
    ships$length_m[j] <- jlength
    
    # ship area (size)
    jarea <- st_area(pol) %>% as.numeric()
    ships$ship_area[j] <- jarea 
  }
  
  # export to data.frame
  shipsDF <- ships %>%
    # remove polygon geometry
    st_drop_geometry() %>%
    # rename ID
    rename(item_id = imageID)
  
  # append
  data_list[[i]] <- shipsDF
}

# combine results
data <- data.table::rbindlist(data_list, fill=TRUE)
nodata <- data.table::rbindlist(nodata_list, fill=TRUE)

data$acquired <- as.POSIXct(gsub(".*(\\d{8}_\\d{6}).*", "\\1", data$item_id), format="%Y%m%d_%H%M%S")

# add field = digitalizer name + item_id
data$filename <- paste(data$item_id, "_ship_", data$digitizer, sep = "")

# export files
outfile <- paste0(outdir, "shipProc.csv")
write.csv(data, outfile, row.names = FALSE)

outfile <- paste0(outdir, "shipNoData.csv")
write.csv(nodata, outfile, row.names = FALSE)
# ---------------------------


# load cross-validation digitalization result:
data <- read.csv('data/output/cross_validation/shipProc.csv')

# 1) Number of vessel (and status navigation or anchored) ---------------------

result <- data |>
  group_by(item_id, digitizer) |> 
  summarize(
    total_vessels = n(),
    length_m_mean = mean(length_m),
    length_m_sd   = sd(length_m),
    ship_area_mean = mean(ship_area),
    ship_area_sd  = sd(ship_area),
    navigating    = sum(navigationStatus == "navigating"),
    anchored      = sum(navigationStatus == "anchor")
  )

View(result)

# t-student = differences mean
t_test <- t.test(length_m_mean ~ digitizer, data = result)
print(t_test)

# result: t = -0.12607, df = 7.6156, p-value = 0.9029 (p > 0.05)
# No significant differences between lenght digitalizers in this project

#ANOVA - number of vessel between digitalizer
anova <- aov(total_vessels ~ digitizer, data = result)
print(summary(anova))
# no significant differences between number of vessel detected

# ANOVA - lenght between images
anova <- aov(length_m_mean ~ item_id, data = result)
print(summary(anova))
# differences in length mean between images 

# import / save result
write.csv(result, "data/output/cross_validation/result/result_cross_validation.csv")

# ----------------------------




# ------------------------------------------------------------------------------
# 2) Spatial analysis                              -----------------------------
#     2.1) intersect (matches) 
#     2.2) distance between centroids
            # combine files, calculare distance?
            # remove duplicate distances (par of centroides)

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


# dist median between ships digitized: 1.223067 m
