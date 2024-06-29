

# Kernel

# Obtener el area que ocupa el 95% de la densidad de barcos digitalizadas 
# previo a la veda

# Filtrado por mes 8


library(raster)
library(dplyr)
library(MASS)
library(sp)


# 0. load vessel dataset created
data <- read.csv("data/output/shipProc.csv")
data$acquired <- as.POSIXct(data$acquired) 

# ** Extraer datos por mes
data$month <- month(data$acquired)
data <- data %>% filter(month == 8) 


# 1. Clean dataset of duplicated vessels
# Duplicated vessel:
data_duplicated <- data %>% filter(duplicated == TRUE)
# check the number of duplicated
print(paste0(nrow(data_duplicated)," vessels duplicated in the images processed "))
# export df of duplciated vessels
outfile <- paste0(outdir, "shipDuplicated.csv")
write.csv(data_duplicated, outfile, row.names = FALSE)

# Navigating vessel:
data_navigate <- data %>% filter(navigationStatus == "navigating")
# check the number of duplicated
print(paste0(nrow(data_navigate)," vessels navigating in the images processed "))
# export df of navigating vessels
outfile <- paste0(outdir, "shipNavigating.csv")
write.csv(data_navigate, outfile, row.names = FALSE)


# Data cleaned
data <- data %>% 
  filter(duplicated == FALSE) %>%
  filter(navigationStatus == "anchor")



# Kernel and plot -------------------------------------------------------------
# number of pixel
num_puntos_x <- 500
num_puntos_y <- 500

k <- kde2d(data$longitude, data$latitude, n = c(num_puntos_x, num_puntos_y))

r <- raster(k)
plot(r)

# quantile 90
qmax <- quantile(r, c(0.95))
# obtain a mask where values are highest than 0.90 percentile
mask <- r >= qmax     # select values >= qmax as a new cells of a new raster (values = 1)
# check
mask[mask == 0] <- NA    # values change 0 by NA (obtain only cells with high marinetraffic area)
#check change
plot(mask)

## Convert raster to polygons
pol <- rasterToPolygons(mask, na.rm = TRUE, dissolve = TRUE)
pol <- st_as_sf(pol)     # convert form SpatialPolygon to sf for export

# expor to polygon as a .gpkg
st_write(pol, "data/output/kernel95_area_filter_ships.gpkg", append = FALSE)



