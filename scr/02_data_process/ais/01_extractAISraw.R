# extractAISraw
# Note this script requires running on a 64-bit version to store the large file
# generated 



source("setup.R")

source("../socib-ais/scr/processing_tools.R")  # sources from external repo
source("../movemed-multispecies/scr/fun_track_proc.R") #  timedif.segment
source("../caldio-interactions/scr/fun_interact.R")

#-----------------------------------------------------------------
# Set paths
#-----------------------------------------------------------------

# Output data folder
output_data <- paste0(output_dir, "/ais/")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



#-----------------------------------------------------------------
# Set params
#-----------------------------------------------------------------

## Prepare clusters
cores <- 16 # detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set full time series for hydrophone
#---------------------------------------------------------------

# import time series
infile <- paste0(input_dir, "/scene_unique_timestamps.csv")
ts <- read.csv(infile)
dates <- as.Date(unique(ts$day))

# Define bounding box
xmax = 2.753003 + 0.05
xmin = 2.708374 - 0.05
ymax = 39.465946 + 0.05
ymin = 39.422730 - 0.05

#---------------------------------------------------------------
# 2. Check AIS database
#---------------------------------------------------------------

# Check dates with data
calendar_file <- "../socib-ais/out/ais_daily_L0.csv"
dates <- dates[check_dates(dates, file = calendar_file)] # filter dates by AIS availability



#---------------------------------------------------------------
# 3. Extract AIS data
#---------------------------------------------------------------


ais_data <- rbindlist(foreach(i=1:length(dates), .packages = c("lubridate", "dplyr"))  %dopar% {
  
  # Import AIS
  year <- year(dates[i])
  data <- import_ais(date = dates[i], path_dyn = "D:/Data/AIS/socib/L0/dynamic/", path_sta = paste0("D:/Data/AIS/socib/L1/static/", year, "/"))
  
  # Exclude codes outside the correct numerical range
  # (i.e. keep MMSI codes with first digits between 2 and 7)
  data <- filter(data, mmsi >= 200000000, mmsi < 800000000)
  
  # exclude anchored or moored vessels
  # data <- filter(data, !status %in% c(1,5), speed >  1.852)
  
  # exclude some types
  exclude_type_summary <- c("", "NULL", "Wing in Grnd")
  exclude_type_name <- c("SAR Aircraft")
  data <- filter(data, !ais_type_summary %in% exclude_type_summary, !type_name %in% exclude_type_name)
  
  ## Subset AIS data by extent & time
  data <- data %>%
    dplyr::filter(lon >= xmin & lon <= xmax & lat >= ymin & lat <= ymax)
  
  # reclass categories
  data$type <- NA
  data$type[data$ais_type_summary %in% c("High Speed Craft", "Passenger")] <- "Passenger"
  data$type[data$ais_type_summary %in% c("Other", "Unspecified", "Search and Rescue", "Special Craft", "Tug")] <- "Other"
  data$type[data$ais_type_summary %in% c("Sailing Vessel", "Pleasure Craft")] <- "Recreational"
  data$type[data$ais_type_summary %in% c("Cargo")] <- "Cargo"
  data$type[data$ais_type_summary %in% c("Tanker")] <- "Tanker"
  data$type[data$ais_type_summary %in% c("Fishing")] <- "Fishing"

  data
  
}, fill=TRUE)


## export data
write.csv(ais_data, paste0(output_data, "aisRaw.csv"), row.names = TRUE)

