# ----------------------------------------------------
# 01 - Other process 
# ---------------------------------------------------

# 1) Process posidonia's cover in Palma Bay 
#     Simplify, changes values, and reduce file size

# Data: https://ideib.caib.es/geoserveis/services/public/GOIB_Posidonia_IB/MapServer/WMSServer


library(dplyr)
library(raster)
library(sf)
library(sp)

r <- raster("data/gis/posidonia/posidonia_raor.tif")

# change values of raster
r[r==0] <- NA
r[r>0] <- 1 

# rasterize to polygon
t <- Sys.time()
v <- rasterToPolygons(r, na.rm = TRUE, dissolve = TRUE)
Sys.time() - t

# change to sf object from SpatialObject (sp)
v_sf <- st_as_sf(v)
plot(v_sf)

# export raster and polygon with new values of presence/absence of posidonia
writeRaster(r, "data/gis/posidonia/posidonia_raor.tif", overwrite = TRUE)
st_write(v_sf, "data/gis/posidonia/posidonia_raor.gpkg", append = FALSE)


