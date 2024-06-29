
#------------------------------------------------------------------------------
# 1. Obtain vessels possition from polygons digitalized
#------------------------------------------------------------------------------

# GitHub: @jmenblaz

# This scipt extract the centroid of the polygons as coordinates
# and extact the tiemstamp and length (long side) of each polygon as a boat length

library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)


#----------------------------------------------------------
# 1. Set data paths
#----------------------------------------------------------
# images satellite data folder
indir <- "data/planet/"

# output directory
outdir <- "data/output/"

#----------------------------------------------------------
# 2. Custom functions
#----------------------------------------------------------
# Run this functions:

# Function to split polygon into lines
splitPolygon <- function(x){
  st_segment = function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}
  m <- data.frame(st_coordinates(x)[, 1:2])
  nr <- nrow(m)
  m2 <- cbind(m[-nr,], m[-1,])
  names(m2) <- c("Xstart", "Ystart", "Xend", "Yend")
  m3 <- rbind(m2,
              data.frame(Xstart = m$Xend[nr],
                         Ystart = m$Yend[nr],
                         Xend = m$Xend[1],
                         Yend = m$Yend))
  m3$geom = st_sfc(sapply(1:nrow(m3), 
                          function(i){st_segment(m3[i,])},simplify=FALSE))
  st_sf(m3, crs = st_crs(x))
}

# Function to get the longest side
longest_side_of_polygon <- function(x){
  df <- splitPolygon(x)
  l <- st_length(df)
  lmat <- st_coordinates(df[which.max(l), ])[,1:2]
  st_sfc(st_linestring(rbind(lmat[1,], lmat[2,])),
         crs = st_crs(x))
}




#----------------------------------------------------------
# 3. Batch processing
#----------------------------------------------------------

# list all files containing cloudfree data
shipFiles <- list.files(indir, pattern = "ship.gpkg$", recursive = TRUE, full.names = TRUE) # "recursive=TRUE" searches all folders / subfolders

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

# get acquired time
#data$date <- gsub( ".*(\\d{8}).*", "\\1", data$item_id)
#data$tformat <- str_detect(data$item_id, pattern="T")
#tsel <- which(data$tformat == TRUE)
#data$time[tsel] <- gsub( ".*T(\\d{6}).*", "\\1", data$item_id[tsel])
#data$time[-tsel] <- gsub( ".*_(\\d{6})_.*", "\\1", data$item_id[-tsel])
#data$time <- gsub( ".*_(\\d{6})_.*", "\\1", data$item_id)
#data$acquired <- parse_date_time(paste(data$date, data$time), "Ymd HMS", tz="UTC")
#data <- dplyr::select(data, -date, -tformat, -time)

data$acquired <- as.POSIXct(gsub(".*(\\d{8}_\\d{6}).*", "\\1", data$item_id), format="%Y%m%d_%H%M%S")

# export files
outfile <- paste0(outdir, "shipProc.csv")
write.csv(data, outfile, row.names = FALSE)

outfile <- paste0(outdir, "shipNoData.csv")
write.csv(nodata, outfile, row.names = FALSE)
