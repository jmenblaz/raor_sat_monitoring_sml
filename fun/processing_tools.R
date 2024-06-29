#-----------------------------------------------------------------------------
# process_raw_mt      Suite of functions to process MarineTraffic API data
#-----------------------------------------------------------------------------
# ais_per_day     Returns number of AIS messages per day
# ais_per_day_L0  Returns number of AIS messages per day from L0
# check_dates     Check dates (comes from ais-anchor)
# filter_ais      Filter AIS based on multiple criteria
# import_ais      Import AIS data combining static and dynamic information  (comes from ais-anchor)
# mtv1L0          Process MarineTraffic API v1 data to generate L0 product
# mtv1proc        Process MarineTraffic API v1 file
# mtv2L0          Process MarineTraffic API v2 data to generate L0 product
# mtv2proc        Process MarineTraffic API v2 files (simple & extended)
# mtv2procext     Process MarineTraffic API v2 files (extended only)
# process_raw     Process raw data
# process_static  Process static data (L0 to L1)
#-----------------------------------------------------------------------------



#-----------------------------------------------------------------------
# ais_per_day         Returns number of AIS messages per day
#-----------------------------------------------------------------------
ais_per_day <- function(date){
  # This function inspects all files for a given date and return the total number of lines,
  # assuming that each line correspond to one AIS message.
  
  # select folder from file system
  # if date is from APIv1
  if (date < "2015-01-01"){
    pat <- format(date, "%Y-%m-%d")  # create time pattern for MTV1
    year <- format(date, "%Y")
    month <- format(date, "%m")
    repo <- paste(mt_paths$path_mtapi_v1, year, month, sep="/")
  }
  # if date is from APIv2
  if (date >= "2015-01-01"){
    pat <- format(date, "%Y%m%d")  # create time pattern for MTV2
    year <- format(date, "%Y")
    month <- format(date, "%m")
    repo <- paste(mt_paths$path_mtapi_v2_simple, year, month, sep="/")
  }  
  
  # import data
  files <- list.files(repo, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                      pattern=pat)  # get file names
  
  # if files length is 0
  if (length(files) == 0) ais <- 0
  
  # count lines 
  if (length(files) > 0){
    lines <- sapply(files, countLines)
    ais <- sum(lines)
  }
  
  # return ais
  return(ais)
}
#-----------------------------------------------------------------------



#-----------------------------------------------------------------------
# ais_per_day_L0         Returns number of AIS messages per day from L0
#-----------------------------------------------------------------------
ais_per_day_L0 <- function(date){
  # This function inspects all files for a given date and return the total number of lines,
  # assuming that each line correspond to one AIS message.
  
  # select folder from file system
  pat <- format(date, "%Y%m%d")  # create time pattern for MTV2
  year <- format(date, "%Y")
  month <- format(date, "%m")
  repo <- paste(socib_paths$path_socib_L0_dyn, year, month, sep="/")
  
  
  # import data
  files <- list.files(repo, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                      pattern=pat)  # get file names
  
  # if files length is 0
  if (length(files) == 0) ais <- 0
  
  # count lines 
  if (length(files) > 0){
    lines <- sapply(files, countLines)
    ais <- sum(lines)
  }
  
  # return ais
  return(ais)
}
#-----------------------------------------------------------------------



#---------------------------------------------------------------------
# check_dates       Check dates
#---------------------------------------------------------------------
check_dates <- function(dates, file = "out/ais_daily.csv"){
  
  library(dplyr)
  
  # Import Calendar file from AIS database
  calendar <- read.csv(file)
  calendar$date <- as.Date(calendar$date)
  
  # Filter dates with data
  calendar <- dplyr::filter(calendar, obs > 300)  # less than 300 rows per day mean empty data
  
  # check dates that match AIS database
  check <- dates %in% calendar$date
  return(check)  
}
#---------------------------------------------------------------------




#---------------------------------------------------------------------
# filter_ais        Filter AIS based on multiple criteria
#---------------------------------------------------------------------
filter_ais <- function(df, e = NULL, fmmsi = NULL, min.records = NULL,
                       include.type = NULL, exclude.type = NULL,
                       min.speed = NULL, max.speed = NULL){
  
  library(dplyr)
  
  # Filer data by bounding box  
  if (!is.null(e)){  # bbox
    xmin <- e@xmin
    xmax <- e@xmax
    ymin <- e@ymin
    ymax <- e@ymax
    df <- dplyr::filter(df, lon >= xmin & lon <= xmax & lat >= ymin & lat <= ymax)
  }
  
  # filter by mmsi
  if (!is.null(fmmsi)) df <- dplyr::filter(df, mmsi %in% fmmsi)  # select by mmsi
  
  # filter by speed
  if (!is.null(min.speed)) df <- dplyr::filter(df, speed >= min.speed)  # select by mmsi
  if (!is.null(max.speed)) df <- dplyr::filter(df, speed <= max.speed)  # select by mmsi
  
  
  # filter by ship type
  if (!is.null(include.type)) df <- dplyr::filter(df, ship_type %in% include.type)  # select by ship_type
  if (!is.null(exclude.type)) df <- dplyr::filter(df, !ship_type %in% exclude.type)  # select by ship_type
  
  # filter by minim number of records
  if (!is.null(min.records)){
    # select mmsi
    mmsi_list <- df %>%
      group_by(mmsi) %>%
      summarise(n = n())
    mmsi_list <- dplyr::filter(mmsi_list, n >= min.records)  # at least 4 positions per mmsi
    # filter by select mmsi
    df <- dplyr::filter(df, mmsi %in% mmsi_list$mmsi) 
  }
  
  # return filtered data.frame
  return(df)
}
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# import_ais        Import AIS data combining static and dynamic information
#---------------------------------------------------------------------
import_ais <- function(date, path_dyn, path_sta){
  # AIS data is stored on daily files, with static and dynamic data stored
  # separately. This function imports both files and combines them given
  # a date.
  # The output is a data.frame with the AIS data
  
  # select dynamic file to process based on date
  pat <- paste0("L0_dyn_",format(date, "%Y%m%d"))  # create time pattern
  dynfile <- list.files(path_dyn, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                        pattern=pat)  # get file names
  
  ## select static file to get ship information
  pat <- "static.csv"
  #path_sta_year <- paste(path_sta ,format(date, "%Y"), sep = "/")
  stafile <- list.files(path_sta, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                        pattern=pat)
  
  # import AIS data and merge
  dyn <- read.csv(dynfile)
  sta <- read.csv(stafile)
  df <- merge(dyn, sta, all.x = TRUE, by=c("mmsi"))
  
  # set POSIXct format
  df$timestamp <- parse_date_time(df$timestamp, "ymd HMS", tz = "UTC")
  
  # return data.frame
  return(df)
}
#---------------------------------------------------------------------



#-----------------------------------------------------------------------
# mtv1L0    Process MarineTraffic API v1 data to generate L0 product
#-----------------------------------------------------------------------
mtv1L0 <- function(date, inrepo, outrepo_dynamic, outrepo_static){
  # Description
  # This function processes a batch of RAW files from MarineTraffic API v1 and prepares a dynamic and static L0 product.
  # The batch is define by a date, and the function selects all files from that day.
  # It depends on mtv1proc(), which is used to process individual files.
  # 
  # Usage
  # mtv1L0(dateClass, repo = "C:/aisdata/mtapi_v1")
  #
  # Arguments
  # date      Date Class
  # repo      File repository root
  #
  # Details
  # Use mtv1L0() for dates between 2011-09-01 and 2014-12-31
  # Use mtv2L0() for dates since 2015-07-01 onwards
  # The processing is based on the following steps:
  # 1. Select all files for day(d)
  # 2. Process individual files and join them into a single table
  # 3. Extract information to create a dynamic table (order by time stamp)
  # 4. Extract information to create a static table (order by mmsi)
  # 5. Remove duplicates where ship_name == mmsi.
  # 6. Create output directory in repository and export two files (static & dynamic)
  #
  # Note
  # About line: dfl <- lapply(files, mtv1proc, land, port)
  # This part takes some time, and a more efficient alternative should be developed. Try parallel approaches or similar.
  #
  # Currently, selection of files is define by 'date' and 'repo' arguments. We could make this process outside the function,
  # and use a 'files' argument. Therefore, we could redefine temporal aggregations more easily.
  #
  #
  # Usage
  # # Load external data
  # library(maps)
  # land <- map("world", fill = TRUE, plot = FALSE)
  # land <- map2SpatialPolygons(land, IDs=land$names,proj4string=CRS("+proj=longlat +ellps=WGS84"))
  # port<-readShapePoly("data/common/bufport.shp")
  # proj4string(port)<- CRS("+proj=longlat +datum=WGS84")
  # # Set remaining arguments
  # date <- as.Date("2014-08-01")
  # repo <- "C:/aisdata/mtapi_v1"
  # mtv1L0(date, repo, land, port)
  #
  # Last changes:
  # Instead a single repo argument, we now allow a input and output repository.
  
  
  ## Load libraries
  library(pbapply)
  
  ## Select files to process based on date
  pat <- format(date, "%Y-%m-%d")  # create time pattern
  files <- list.files(inrepo, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                      pattern=pat)  # get file names
  
  ## if files length is 0, stop
  if (length(files) == 0) stop("No files found")
  
  ## Process files and mae on single table
  dfl <- pblapply(files, mtv1proc)  # create a list of data.tables. This process can take some time.
  df <- rbindlist(dfl)  # make on data.table
  
  # create dynamic table
  dyn <- df %>% 
    select(mmsi, timestamp, lon, lat, speed, course, status) %>% # select variables
    arrange(timestamp) # order by time stamp
  
  
  # create static table
  # Count number of unique combinations on static data
  sta <- df %>% 
    group_by(mmsi, ship_name, ship_type) %>%
    tally()
  
  # check mmsi duplicates on static
  # there are duplicates where ship_name == mmsi. In that case, we remove it
  # in other cases, we keep them in L0. Then, we will gather all static and keep the most frequent combination
  # note as well that some ships can have their names == mmsi but no duplicates. Check them in L1
  dups <- which((sta$mmsi == sta$ship_name & (duplicated(sta$mmsi) | duplicated(sta$mmsi, fromLast = TRUE))))
  if (length(dups)>0) sta <- sta[-dups,]
  
  # create output directory if does not exist
  out.dyn <- paste(outrepo_dynamic, format(date, "%Y"), format(date, "%m"), sep="/")
  out.sta <- paste(outrepo_static, format(date, "%Y"), format(date, "%m"), sep="/")
  if (!dir.exists(out.dyn)) dir.create(out.dyn, recursive = TRUE)
  if (!dir.exists(out.sta)) dir.create(out.sta, recursive = TRUE)
  
  ## export files
  file.d <- paste0("L0_dyn_",format(date, "%Y%m%d"),".csv")
  file.s <- paste0("L0_sta_",format(date, "%Y%m%d"), ".csv")
  write.csv(dyn, file = paste(out.dyn, file.d, sep="/"), row.names=FALSE)
  write.csv(sta, file = paste(out.sta, file.s, sep="/"), row.names=FALSE)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# mtv1proc    Process MarineTraffic API v1 file
#-----------------------------------------------------------------------
mtv1proc <- function(file){
  # Description
  # This function processes individual RAW files from MarineTraffic API v1. For general use, this function is used in mtv1L0.R.
  #
  # Usage
  # mtv1proc("ais_marinetraffic_2014-08-01_00_00_01_UTC.kml~1.csv", land, port)
  #
  # Details
  # The processing is based on the following steps:
  # 1. Rename variables to a common format
  # 2. Recode "status"
  # 3. Recode "ship type"
  # 4. Filter duplicates
  # 5. Keep registers where MMSI code = 9digits
  # 6. Unit conversion: speed (knots to km/h)
  # 7. Create new variables: onland, onport
  # 8. Reorder and select variables
  #
  # Examples
  # library(maps)
  # land <- map("world", fill = TRUE, plot = FALSE)
  # land <- map2SpatialPolygons(land, IDs=land$names,proj4string=CRS("+proj=longlat +ellps=WGS84"))
  # port<-readShapePoly("data/common/bufport.shp")
  # proj4string(port)<- CRS("+proj=longlat +datum=WGS84")
  # data <- mtv1proc("C:/aisdata/mtapi_v1/csv/2014/08/ais_marinetraffic_2014-08-01_00_00_01_UTC.kml~1.csv", land, port)
  
  
  ## Load libraries
  library(dplyr)
  library(data.table)
  library(stringr)
  
  ### import data from file   
  data <- fread(file, sep = ";", dec = ".", header = TRUE)
  
  ### rename variables to common socib format
  names(data)[names(data)=="sNombre"] <- "ship_name"
  names(data)[names(data)=="iMMSI"] <- "mmsi"
  names(data)[names(data)=="sTipo"] <- "ship_type"
  names(data)[names(data)=="iSOG"] <- "speed"
  names(data)[names(data)=="iCOG"] <- "course"
  names(data)[names(data)=="dLatitud"] <- "lat"
  names(data)[names(data)=="dLongitud"] <- "lon"
  names(data)[names(data)=="sEstado"] <- "status"
  names(data)[names(data)=="dtFecha"] <- "timestamp"
  
  ### rename status data to common socib format
  data$status <- as.character(data$status)
  data$status[data$status == "Underway"] <- 0
  data$status[data$status == "Anchored"] <- 1
  data$status[data$status == "Fishing"] <- 7
  
  ### rename ship type data to common socib format
  data$ship_type <- as.character(data$ship_type)
  data$ship_type[data$ship_type == "Fisher"] <- 30
  data$ship_type[data$ship_type == "Yatchs & Special Craft"] <- 36  # or 59?
  data$ship_type[data$ship_type == "High Speed Craft"] <- 40
  data$ship_type[data$ship_type == "Tug, Pilot, etc"] <- 52
  data$ship_type[data$ship_type == "Passenger"] <- 60
  data$ship_type[data$ship_type == "Cargo"] <- 70
  data$ship_type[data$ship_type == "Tanker"] <- 80
  data$ship_type[data$ship_type == "Navigation Aid"] <- 100
  data$ship_type[data$ship_type == "Unspecified"] <- 99  # check this
  
  ### Filtering
  data <- distinct(data)  # filter duplicates
  data <- filter(data, str_length(mmsi) == 9)  # only get mmsi with 9digits

  ### Units conversion
  data$speed <- data$speed * 1.852  # speed (knots to km/h)
  
  ### Data preparation
  data <- select(data, mmsi, timestamp, lon, lat, speed, course, status,
                 ship_name, ship_type)  # reorder and select
  return(data)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# mtv2L0    Process MarineTraffic API v2 data to generate L0 product
#-----------------------------------------------------------------------
mtv2L0 <- function(date, inrepo, outrepo_dynamic, outrepo_static){
  # Description
  # This function processes a batch of RAW files from MarineTraffic API v2 and prepares a dynamic and static L0 product.
  # The batch is defined by a date, and the function selects all files from that day.
  # It depends on mtv2proc() and mtv2procext(), which is used to process individual files.
  # 
  # Usage
  # mtv1L0(dateClass, repo = "C:/aisdata/mtapi_v2")
  #
  # Arguments
  # date      Date Class
  # repo      File repository root
  #
  # Details
  # The processing is based on the following steps:
  # 1. Select all files for day(d)
  # 2. Dynamic: Process individual files (simple & extended) and join them into a single table
  # 3. Static: Process individual files (extended only) and join them into a single table
  # 4. Static: Remove duplicates where ship_name == mmsi.
  # 5. Create output directory in repository and export two files (static & dynamic)
  #
  # Issues found
  # - There are timestamps in files for day(i), which may correspond to the day after or before. Solutions:
  # i) discard those points (not many), ii) process files from day i+-1. At this step, we selection option (i)
  
  
  # Examples
  
  # # Load external data
  # library(maps)
  # library(maptools)
  # land <- map("world", fill = TRUE, plot = FALSE)
  # land <- map2SpatialPolygons(land, IDs=land$names,proj4string=CRS("+proj=longlat +ellps=WGS84"))
  # port<-readShapePoly("data/common/bufport.shp")
  # proj4string(port)<- CRS("+proj=longlat +datum=WGS84")
  # 
  # # Set remaining arguments
  # date_start <- as.Date("2017-06-21")
  # date_end <- as.Date("2017-06-21")
  # dates <- seq(date_start, date_end, by="day")
  # repo <- "D:/aisdata/tortugas"
  # 
  # for (i in 1:length(dates)){
  #   print(paste("Day", i, "from", length(dates)))
  #   date <- dates[i]
  #   mtv2L0(date, repo, land, port)
  # }
  
  
  ## Load libraries
  library(pbapply)
  library(dplyr)
  library(log4r)
  
  ## Create log
  logger <- create.logger()  # Create a new logger object
  logfile(logger) <- file.path("log/mtv2L0.log")  # Set the logger's file output
  level(logger) <- "INFO"  # Set the current level of the logger.
  
  ## Select files to process based on date
  pat <- format(date, "%Y%m%d")  # create time pattern
  files <- list.files(inrepo, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                      pattern=pat)  # get file names
  
  ## if files length is 0, stop
  if (length(files) == 0) stop("No files found")
  info(logger, paste0("Data processing L0. Day: ", date, ". Total files: ", length(files)))
  
  # Dynamic --------------------------
  # Process files and make on single table for dynanic data
  # This is the tricky part
  # It could happen to find empty files. Therefore, is better to use:
  # - Trycatch to evaluate the function. Will return NA if does not work.
  # - Subset data.frame list by filtering NAs
  tryproc <- function(files){tryCatch(mtv2proc(files),error=function(e){NA})}
  dfl <- pblapply(files, tryproc) 
  if (any(is.na(dfl))){
    warn(logger, paste0("Data processing L0 - Dynamic empty file found ", files[which(is.na(dfl))]))  # report log
    dfl <- dfl[-which(is.na(dfl))]  # remove NA elements if any
  }   
  dyn <- rbindlist(dfl)  # make on data.table
  
  # keep only data from current date (filter out dates from previous or following day)
  dyn <- filter(dyn, as.Date(timestamp) == date)
  
  # order data by timestamp (two options but takes time, not used yet)
  # arrange(dyn, timestamp)
  # dyn[order(dyn$timestamp),]
  
  # remove duplicates
  dyn <- dyn[!duplicated(dyn, by=c("mmsi", "timestamp", "lon", "lat")),]
  
  
  
  # Static ----------------------------
  # Locate extended files
  ext <- grep("extended", files)  # identify which files are extended files
  files <- files[ext]  # get extended files
  tryproc <- function(files){tryCatch(mtv2procext(files),error=function(e){NA})}
  dfl <- pblapply(files, tryproc) 
  if (any(is.na(dfl))){
    warn(logger, paste0("Data processing L0 - Static empty file found ", files[which(is.na(dfl))]))  # report log
    dfl <- dfl[-which(is.na(dfl))]  # remove NA elements if any
  }   
  sta <- rbindlist(dfl)  # make on data.table
  
  # check mmsi duplicates on static
  # there are duplicates where ship_name == mmsi. In that case, we remove it
  # in other cases, we keep them in L0. Then, we will gather all static and keep the most frequent combination
  # note as well that some ships can have their names == mmsi but no duplicates. Check them in L1
  dups <- which((sta$mmsi == sta$ship_name & (duplicated(sta$mmsi) | duplicated(sta$mmsi, fromLast = TRUE))))
  if (length(dups)>0) sta <- sta[-dups,]
  
  # Count number of unique combinations on static data
  sta <- sta %>%
    group_by(mmsi, ship_name, ship_type, callsign, imo, type_name, ais_type_summary, flag, length, width, grt, dwt) %>%
    tally()

  
  # Export tables---------------------------
  # create output directory if does not exist
  out.dyn <- paste(outrepo_dynamic, format(date, "%Y"), format(date, "%m"), sep="/")
  out.sta <- paste(outrepo_static, format(date, "%Y"), format(date, "%m"), sep="/")
  if (!dir.exists(out.dyn)) dir.create(out.dyn, recursive = TRUE)
  if (!dir.exists(out.sta)) dir.create(out.sta, recursive = TRUE)
  
  ## export files
  file.d <- paste0("L0_dyn_",format(date, "%Y%m%d"),".csv")
  file.s <- paste0("L0_sta_",format(date, "%Y%m%d"), ".csv")
  write.csv(dyn, file = paste(out.dyn, file.d, sep="/"), row.names=FALSE)
  write.csv(sta, file = paste(out.sta, file.s, sep="/"), row.names=FALSE)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# mtv2proc    Process MarineTraffic API v2 files (simple & extended)
#-----------------------------------------------------------------------
mtv2proc <- function(file){
  # Description
  # This function processes individual RAW files (simple & extended) from MarineTraffic API v2. For general use, this function is used in mtv2L0.R.
  # Simple files contain dynamic information every 5 minutes
  # Extended files contain dynamic & static information every 1 hour. They contain metadata about ships
  # It depends on onland() and onport() functions.
  #
  # Usage
  # mtv2proc("ais_mtapi_20161101_000000_05min_simple.csv", land, port)
  #
  # Details
  # The processing is based on the following steps:
  # 1. Rename variables to a common format
  # 4. Filter duplicates
  # 5. Keep registers where MMSI code = 9digits
  # 6. Unit conversion: speed (knotsx10 to km/h)
  # 7. Create new variables: onland, onport
  # 8. Recode time format
  # 9. Reorder and select variables
  #
  # Examples
  # library(maps)
  # land <- map("world", fill = TRUE, plot = FALSE)
  # land <- map2SpatialPolygons(land, IDs=land$names,proj4string=CRS("+proj=longlat +ellps=WGS84"))
  # port<-readShapePoly("data/common/bufport.shp")
  # proj4string(port)<- CRS("+proj=longlat +datum=WGS84")
  # file <- "C:/aisdata/mtapi_v2/mtapi_simple/2016/11/ais_mtapi_20161101_000000_05min_simple.csv"
  # file <- "C:/aisdata/mtapi_v2/mtapi_extended/2016/11/ais_mtapi_20161101-000000_60min_extended.csv"
  # data <- mtv2proc(file, land, port)
  
  
  ## Load libraries
  library(dplyr)
  library(data.table)
  library(stringr)
  library(sp)
  
  ### import data from file   
  data <- fread(file, sep = ",", dec = ".", header = TRUE)
  if (nrow(data)==0) stop ("no data")
  
  ### select and rename variables to common socib format
  names(data)[names(data)=="MMSI"] <- "mmsi"
  names(data)[names(data)=="SPEED"] <- "speed"
  names(data)[names(data)=="COURSE"] <- "course"
  names(data)[names(data)=="HEADING"] <- "heading"
  names(data)[names(data)=="LAT"] <- "lat"
  names(data)[names(data)=="LON"] <- "lon"
  names(data)[names(data)=="STATUS"] <- "status"
  names(data)[names(data)=="TIMESTAMP"] <- "timestamp"
  
  ### Filtering
  data <- distinct(data)  # filter duplicates
  data <- filter(data, str_length(mmsi) == 9)  # only get mmsi with 9digits
  
  ### Units conversion
  data$speed <- (data$speed/10) * 1.852  # speed (knotsx10 to km/h)
  
  ### Create projected coordinates (EPSG:3035)
  data.proj <- data
  coordinates(data.proj)= ~lon + lat   # convert to class spatial
  proj4string(data.proj) <- CRS("+init=epsg:4326")    # define coordinate system
  data.proj <- spTransform(data.proj, CRS=CRS("+init=epsg:3035"))  # transform to other CRS
  data$x <- data.proj$lon
  data$y <- data.proj$lat
  
  ### rearange time format
  data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%dT%H:%M:%S", tz="UTC") 
  
  ### Data preparation
  data <- select(data, mmsi, timestamp, lon, lat, speed, course, status,
                 heading)  # reorder and select
  return(data)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# mtv2procext    Process MarineTraffic API v2 files (extended only)
#-----------------------------------------------------------------------
mtv2procext <- function(file){
  # Description
  # This function processes individual RAW files (extended only) from MarineTraffic API v2. For general use, this function is used in mtv2L0.R.
  # Extended files contain dynamic & static information every 1 hour. They contain metadata about ships
  #
  # Usage
  # mtv2procext("ais_mtapi_20161101-000000_60min_extended.csv")
  #
  # Details
  # The processing is based on the following steps:
  # 1. Rename variables to a common format
  # 2. Filter duplicates
  # 3. Keep registers where MMSI code = 9digits
  # 4. Reorder and select variables
  #
  # Examples
  # file <- "C:/aisdata/mtapi_v2/mtapi_extended/2016/11/ais_mtapi_20161101-000000_60min_extended.csv"
  # data <- mtv2procext(file)
  
  ## Load libraries
  library(dplyr)
  library(data.table)
  library(stringr)
  
  ### import data from file   
  data <- fread(file, sep = ",", dec = ".", header = TRUE)
  if (nrow(data)==0) stop ("no data")
  
  ### select and rename variables to common socib format
  names(data)[names(data)=="SHIPNAME"] <- "ship_name"
  names(data)[names(data)=="SHIPTYPE"] <- "ship_type"
  names(data)[names(data)=="TYPE_NAME"] <- "type_name"
  names(data)[names(data)=="AIS_TYPE_SUMMARY"] <- "ais_type_summary"
  names(data)[names(data)=="MMSI"] <- "mmsi"
  names(data)[names(data)=="CALLSIGN"] <- "callsign"
  names(data)[names(data)=="IMO"] <- "imo"
  names(data)[names(data)=="FLAG"] <- "flag"
  names(data)[names(data)=="LENGTH"] <- "length"
  names(data)[names(data)=="WIDTH"] <- "width"
  names(data)[names(data)=="GRT"] <- "grt"
  names(data)[names(data)=="DWT"] <- "dwt"
  
  ### Filtering
  data <- distinct(data)  # filter duplicates
  data <- filter(data, str_length(mmsi) == 9)  # only get mmsi with 9digits
  
  ### Data preparation
  data <- select(data, mmsi, callsign, imo, ship_name, ship_type, type_name, ais_type_summary, flag, length, width, grt, dwt)  # reorder and select
  return(data)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# mtv2procHist    Process MarineTraffic Historical Data
#-----------------------------------------------------------------------
mtv2procHist <- function(data){
  # Description
  # This function processes historical data from MarineTraffic API v2.
  # Despite mtv2proc() the input of this function is not a file but a data.frame with
  # historical AIS data.
  # This function returns a list with 2 data.frames, one with dynamic information, the other with static.
  #
  # Details
  # The processing is based on the following steps:
  # 1. Rename variables to a common format
  # 2. Filter duplicates
  # 3. Keep registers where MMSI code = 9digits
  # 4. Process dynamic data
  # 4.1. Unit conversion: speed (knotsx10 to km/h)
  # 4.2. Create new variables: x,y
  # 4.3. Reorder and select variables
  # 5. Process static data
  # 5.1. Remove duplicates
  # 5.2. Get unique values
  # 6. Export list with dynamic and static data.
  
  ## Load libraries
  library(dplyr)
  library(data.table)
  library(stringr)
  library(sp)
  
  ### select and rename variables to common socib format
  names(data)[names(data)=="MMSI"] <- "mmsi"
  names(data)[names(data)=="SPEED"] <- "speed"
  names(data)[names(data)=="COURSE"] <- "course"
  names(data)[names(data)=="HEADING"] <- "heading"
  names(data)[names(data)=="LAT"] <- "lat"
  names(data)[names(data)=="LON"] <- "lon"
  names(data)[names(data)=="STATUS"] <- "status"
  names(data)[names(data)=="TIMESTAMP"] <- "timestamp"
  names(data)[names(data)=="SHIPNAME"] <- "ship_name"
  names(data)[names(data)=="SHIPTYPE"] <- "ship_type"
  names(data)[names(data)=="TYPE_NAME"] <- "type_name"
  names(data)[names(data)=="AIS_TYPE_SUMMARY"] <- "ais_type_summary"
  names(data)[names(data)=="CALLSIGN"] <- "callsign"
  names(data)[names(data)=="IMO"] <- "imo"
  names(data)[names(data)=="FLAG"] <- "flag"
  names(data)[names(data)=="LENGTH"] <- "length"
  names(data)[names(data)=="WIDTH"] <- "width"
  names(data)[names(data)=="GRT"] <- "grt"
  names(data)[names(data)=="DWT"] <- "dwt"
  
  ### Filtering
  data <- distinct(data)  # filter duplicates
  data <- filter(data, str_length(mmsi) == 9)  # only get mmsi with 9digits
  
  
  ### PROCESS DYNAMIC ---------------------------------------------------------
  
  ### Units conversion
  data$speed <- (data$speed/10) * 1.852  # speed (knotsx10 to km/h)
  
  ### Create projected coordinates (EPSG:3035)
  data.proj <- data
  coordinates(data.proj)= ~lon + lat   # convert to class spatial
  proj4string(data.proj) <- CRS("+init=epsg:4326")    # define coordinate system
  data.proj <- spTransform(data.proj, CRS=CRS("+init=epsg:3035"))  # transform to other CRS
  data$x <- data.proj$lon
  data$y <- data.proj$lat
  
  ### Data preparation
  dyn <- dplyr::select(data, mmsi, timestamp, lon, lat, speed, course, status,
                       heading)  # reorder and select
  
  # remove duplicates
  dyn <- dyn[!duplicated(dyn, by=c("mmsi", "timestamp", "lon", "lat")),]
  
  
  ### PROCESS STATIC ---------------------------------------------------------
  
  ### Data preparation
  sta <- dplyr::select(data, mmsi, callsign, imo, ship_name, ship_type, type_name, ais_type_summary, flag, length, width, grt, dwt)  # reorder and select
  
  # check mmsi duplicates on static
  # there are duplicates where ship_name == mmsi. In that case, we remove it
  # in other cases, we keep them in L0. Then, we will gather all static and keep the most frequent combination
  # note as well that some ships can have their names == mmsi but no duplicates. Check them in L1
  dups <- which((sta$mmsi == sta$ship_name & (duplicated(sta$mmsi) | duplicated(sta$mmsi, fromLast = TRUE))))
  if (length(dups)>0) sta <- sta[-dups,]
  
  # Count number of unique combinations on static data
  sta <- sta %>%
    group_by(mmsi, ship_name, ship_type, callsign, imo, type_name, ais_type_summary, flag, length, width, grt, dwt) %>%
    tally()
  
  
  ### RETURN LIST WITH DYNAMIC AND STATIC --------------------------
  l <- list(dyn = dyn, sta = sta)
  return(l)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# process_raw     Process raw data
#-----------------------------------------------------------------------
process_raw <- function(dates, path_mtapi_v1, path_mtapi_v2, outrepo_dynamic, outrepo_static){
  # This function performs parallel computing to process batch files from
  # both MarineTraffic API versions
  
  #source("scr/processing_tools.R")
  
  # Register number of cores to use in parallel
  cores <- detectCores()
  cl <- parallel::makeCluster(floor(cores*0.7)) # Use 90% of cores. Alternatively, just leave one cores-1
  registerDoParallel(cl)
  
  # Process raw data
  foreach(i=1:length(dates), .packages=c("lubridate", "stringr"),
          .export=c("mtv1L0", "mtv2L0", "mtv1proc", "mtv2proc", "mtv2procext")) %dopar% {
            
            # get date
            date <- dates[i]
            
            # if date is from APIv1
            if (date < "2015-01-01"){
              inrepo <- paste(path_mtapi_v1, year(date), str_pad(month(date), 2, pad = "0"), sep="/")  # check that this structure is the same for the whole period
              mtv1L0(date, inrepo = inrepo, outrepo_dynamic = outrepo_dynamic, outrepo_static = outrepo_static)
            }
            
            # if date is from APIv2
            if (date >= "2015-01-01"){
              inrepo <- path_mtapi_v2 # check that this structure is the same for the whole period
              mtv2L0(date, inrepo = inrepo, outrepo_dynamic = outrepo_dynamic, outrepo_static = outrepo_static)
            }
          }
  
  # Stop cluster
  parallel::stopCluster(cl)
}
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# process_static     Process static data (L0 to L1)
#-----------------------------------------------------------------------
process_static <- function(inrepo, outrepo, api = "v2"){
  # This function aims to generate a common list of MMSI given several
  # inconsistencies in the data:
  # 1.- Same MMSI for different vesels names or types
  # 2.- Different vessel characteristics for same combination of name and MMSI
  # 3.- Wrong formats of ship types
  # 4.- No data for some fields
  #
  # The approach followed here is to select among duplicates those combinations
  # with a higher number of records.
  #
  # There are several limitations with this approach, that could be improved
  # in the future:
  # - Selects more frequent combination over the most complete
  # - Does not correct inconsistencies in ship type (e.g. string format rather than numeric)
  
  ## Combine multiple files -------------------
  
  # List static files 
  pat= "L0_sta_"
  files <- list.files(inrepo, full.names = TRUE, include.dirs = TRUE, recursive = TRUE,
                      pattern=pat)  # get file names
  
  # Register number of cores to use in parallel
  cores <- detectCores()
  cl <- parallel::makeCluster(floor(cores*0.9)) # Use 90% of cores. Alternatively, just leave one cores-1
  registerDoParallel(cl)
  
  # Combine all files into single table
  # Using fill=TRUE allows combining static files with different columns (i.e. API v1 and v2)
  data <- rbindlist(foreach (i = 1:length(files)) %dopar% read.csv(files[i]), fill = TRUE)
  
  # Stop cluster
  parallel::stopCluster(cl)
  
  
  ## Analyse multiple combinations -------------------
  
  # Get total number of unique combinations
  # If API v2
  if (api == "v1"){
    allcomb <- data %>%
      group_by(mmsi, ship_name, ship_type) %>%
      summarise(n=sum(n))
  }
  # If API v2
  if (api == "v2"){
  allcomb <- data %>%
    group_by(mmsi, callsign, imo, ship_name, ship_type, type_name, ais_type_summary, flag, length, width, grt, dwt) %>%
    summarise(n=sum(n))
  }
  
  ### Select combinations with unique MMSI (uni.mmsi)
  uni.mmsi <- allcomb %>%
    group_by(mmsi) %>%
    filter(n()==1)
  
  ### Select combinations with duplicated MMSI (dup.mmsi)
  dup.mmsi <- allcomb %>%
    group_by(mmsi) %>%
    filter(n()>1)
  
  ### Select among duplicates based on frequency of observation (sel.dup.mmsi)
  sel.dup.mmsi <- dup.mmsi %>%
    group_by(mmsi) %>%
    filter(n==max(n)) %>%
    slice(1)  # if there are more than one register with max(n), select the first one
  
  ### Combine uni.mmsi & sel.dup.mmsi
  static <- rbind(uni.mmsi, sel.dup.mmsi )
  
  ### Select discarded duplicates
  discards <- dup.mmsi %>%
    group_by(mmsi) %>%
    filter(n!=max(n))
  
  
  ## Export files -------------------
  
  # create output directory if does not exist
  #out.dir <- paste0(outrepo, "/static/")
  out.dir <- outrepo
  if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)
  
  # export files
  write.csv(allcomb, file = paste(out.dir, "all_combinations.csv", sep="/"), row.names=FALSE)  # all combinations
  write.csv(static, file = paste(out.dir, "static.csv", sep="/"), row.names=FALSE)  # selected static data
  write.csv(discards, file = paste(out.dir, "discarded_combinations.csv", sep="/"), row.names=FALSE)  # discarded data
}
#-----------------------------------------------------------------------