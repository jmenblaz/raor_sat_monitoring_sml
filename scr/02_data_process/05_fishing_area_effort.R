# --------------------------------------------------
# 05- fishing effort - process
# --------------------------------------------------

# Calculate fishing effort on a daily basis per different method 

# Ships outside non-take area, no navigating, beyond 200m of coastline 
# and outside of Posidonia oceanica cover are considered as a potential fishing vessel


# 0) Create a potential fishing area (fa) within the study area

# 1) Fishing effort

# 1.1) Filter spatial data per day
#   Filter data:
#     - Navigating (speed or status AI)
#     - Duplicated
#     - LowCertainty
#  Spatial filtering; NO ships in potential fishing area (fa):
#     - Non-take area
#     - Buffer 200 m coast
#     - Posidonia cover

# 1.2) Export results
#     - Fishing effort (raster) - vessel/km2 (daily)
#     - df (mean fishing effort per day)

# ----------------------------------------------------------


library(sp)
library(sf)
library(dplyr)
library(raster)


# load spatial data
sa <- read_sf("data/gis/study_area/study_area_raor.gpkg")
mpa  <- read_sf("data/gis/mpa/mpapb.gpkg")
notake_area <- read_sf("data/gis/mpa/mpapb_notake_area.gpkg")
b200 <- read_sf("data/gis/admin_balearic_islands/buffer200_landsmask.gpkg")
posidonia <- read_sf("data/gis/posidonia/posidonia_raor.gpkg")

# assign same CRS
st_crs(notake_area) <- st_crs(mpa)
st_crs(sa) <- st_crs(mpa)
st_crs(b200) <- st_crs(mpa)
st_crs(posidonia) <- st_crs(mpa)

# 0) create potential fishing area or cover (fa) ------------------------------
#     - Non-take area
#     - Buffer 200 m coast
#     - Posidonia cover



# study area as potential fishing area - spatial difference
fa <- sa
#     - Non-take area
fa <- st_difference(fa, notake_area)
#     - Buffer 200 m coast
fa <- st_difference(fa, b200)
#     - Posidonia cover
fa <- st_difference(fa, posidonia)

st_write(fa, "data/gis/fa/fishing_area.gpkg", append = FALSE)
st_area(fa) 
# = 7.69 Km2 of potential fishing area of razorfish
(st_area(fa)/st_area(sa))*100
# 41.77% of the study area correspond to a potential area of fishing razorfish

fa <- st_read("data/gis/fa/fishing_area.gpkg")
st_crs(fa) <- st_crs(mpa)




# 1) Fishing effort ------------------------------------------------------------

# year of study ----------------
years <- 2016:2023


# extension of study area (bounding box)
ext <- st_bbox(sa)

# coordinates max min
y_max <- st_bbox(ext)["ymax"] 
y_min <- st_bbox(ext)["ymin"] 
x_max <- st_bbox(ext)["xmax"] 
x_min <- st_bbox(ext)["xmin"] 

# Create a raster layer for fishing effort == potential fishing ships / area ---
# 50 m of resolution
r <- raster(xmn = x_min, xmx= x_max, ymn= y_min, ymx= y_max,
            crs = CRS("+proj=longlat +datum=WGS84"),
            resolution=c(0.0005,0.0005), vals=NULL)
# calculate area for each cell
r <- area(r)  # km2
r[r>0] <- 50  # 50m2 of area
# writeRaster(r, "data/output/raster/00_area_cell_50m.tif", overwrite=TRUE)



# load and prepare data ---------
# Note*: load instrument data and run step 1.3) Fishing effort process

# 1.1) Satellite data -----------------------------------------------------
method <- "satellite"
df <- read.csv("data/output/shipProc.csv", sep = ";")  # 4847 total
# create day variable in data
df$day <- format(as.Date(df$acquired), "%Y-%m-%d")

# prepare satellite data for fishing effort analysis
df <- df %>% 
  filter(duplicated == FALSE) %>%   
  filter(navigationStatus == "anchor") %>%  
  filter(lowCertainty == FALSE)   


# 1.2) AIS data ----------------------------------------------------------
method <- "ais"
df <- read.csv("data/output/ais_scenes_interpolated.csv")  # 4847 total
# create day variable in data
df$day <- format(as.Date(df$timestamp), "%Y-%m-%d")




# 1.3) Fishing effort process --------------------------------------------------

# empty df
fishing_effort <- data.frame(day = numeric(),
                             year = numeric(),
                             ships = numeric(),
                             ship_dens_fa = numeric())

t <- Sys.time()

for (y in 1:length(years)) {
  year <- years[y]
  # info
  print(paste0("Processing year ", year, " of 2023"))
  days <- seq(as.Date(paste(year, "-08-25", sep = "")), as.Date(paste(year, "-09-20", sep = "")), by = "days")
  
  for (d in 1:length(days)) {
    day <- days[d]
    # filter ships per day
    cnt <- df %>% filter(day == !!day)
    
    # fishing effort - ship density spatial data (raster) -----------
    # get grid cell id for each vessel observation
    cnt$cellID <- cellFromXY(r, cbind(cnt$longitude, cnt$latitude)) 
    cnt$cellID <- as.character(cnt$cellID)
    
    # filter potential
    cnt <- filter(cnt, !is.na(cellID))
    
    # count number ships per cell ID
    cell_all <- cnt %>%
      group_by(cellID) %>%
      summarize(ships = n())
    # as numeric
    cell_all$cellID <- as.numeric(cell_all$cellID) 
    # call to raster base layer
    rcount <- r
    # import ship cell counting to raster by cellID
    rcount[cell_all$cellID] <- cell_all$ships
    # convert to densities
    rcount[rcount == 50] <- 0 # Note that r contain area/extent value 50m
    rcount[rcount == 0] <- NA
    # dens(raster values as num ships / 50m)
    rdens <- rcount/r
    
    # mask data in fishing area (fa)
    rcount <- mask(rcount, fa, inverse = FALSE)
    rdens  <- mask(rdens, fa, inverse = FALSE)
    
    # export raster
    path <- paste0("data/output/fishing_effort/raster/", method,"/count/")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    
    writeRaster(rcount, paste0(path, day, "_count",".tif"), overwrite=TRUE)
    
    path <- paste0("data/output/fishing_effort/raster/", method,"/dens/")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    
    writeRaster(rdens, paste0(path, day,"_dens_ships_50m",".tif"), overwrite=TRUE)
    
    # fishing effort daily cloudpoint -------------------------------
    cnt$lon <- cnt$longitude
    cnt$lat <- cnt$latitude
    ships <- st_as_sf(cnt, coords = c("lon", "lat"), crs = st_crs(mpa))
    # intersect ships within fa
    ships <- st_intersection(ships, fa)
    
    # filter columns by method / instrument
    if (method == "satellite") {
      # transform to df
      ships <- ships %>% 
        st_drop_geometry() %>%
        # select not working without dplyr::
        dplyr::select(item_id:cellID)
    }
    if (method == "ais") {
      ships <- ships %>% 
        st_drop_geometry()
    }
    
    # export cloudpoints
    path <-paste0("data/output/fishing_effort/cloudpoint/",method,"/")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    
    write.csv(ships, paste0(path, day, "_fishig_effort_cloudpoint.csv"), row.names = FALSE)
    
    # fishing effort daily data -----------------------------------------
    ships <- nrow(ships)
    # number of fishing ships / fishing area (Km2)
    ships_dens_fa <- as.numeric((ships/(st_area(fa)/1000000)))
    
    fe_day <- data.frame(timestamp = day,
                         day = format(as.Date(day), "%m-%d"),
                         year = year,
                         ships = ships,
                         ship_denskm2_fa = ships_dens_fa)
    # bind dfs
    fishing_effort <- rbind(fishing_effort, fe_day)
    
  }
}

Sys.time() - t  
# Note: X min for Satellite 12 min
# Note: X min for AIS data

# specify method/instrument
fishing_effort$method <- method

# export fishing effort data
path <- "data/output/fishing_effort/"
write.csv(fishing_effort, paste0(path, method,"_fishing_effort_data.csv"), row.names = FALSE)

