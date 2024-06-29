
# -----------------------------------------------------------------------------
# 4) Fishing effort -----------------------------------------------------------

library(sf)
library(dplyr)
library(gghalves)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggtext)
library(data.table)

# Basemap ---------------------------------------------------------------------
# land, study area (sa), MPA, non-take area and buffer 200 from coastline
sa   <- read_sf("data/gis/study_area/study_area_raor.shp")
land <- read_sf("data/gis/admin_balearic_islands/illesBaleares.shp")
mpa  <- read_sf("data/gis/mpa/mpapb.gpkg")
notake_area <- read_sf("data/gis/mpa/mpapb_notake_area.gpkg")
b200 <- read_sf("data/gis/admin_balearic_islands/buffer200_landsmask.gpkg")
posidonia <- read_sf("data/gis/posidonia/posidonia_raor.gpkg")

# assign same CRS to data_sf
st_crs(ais) <- st_crs(notake_area)
st_crs(ships) <- st_crs(notake_area)
st_crs(posidonia) <- st_crs(notake_area)

# study area extent
sa_extent <- st_bbox(sa)

# create xl and yl object for ggplot
xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) 
yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0))


basemap <- ggplot() +
  # posidonia
  geom_sf(data = posidonia, fill = "#55711c", colour = "grey10", size = 1, alpha = 0.35) +
  # No-take area
  geom_sf(data = notake_area, fill = "#CD5B45", colour = "grey10", size = 9, alpha = 0.75) +
  # land mask
  geom_sf(data = land, fill = "grey25", colour = "black", size = 10, alpha = 0.95) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey10", size = 6) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = F) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_rect(fill = "grey50"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))


basemap






# Note*: Run the first time only
# Prepare cloudpoints data obtained previously --------------------------------
method <- "satellite"
folder <- "data/output/fishing_effort/cloudpoint/satellite/"
method <- "ais"
folder <- "data/output/fishing_effort/cloudpoint/ais/"
method <- "fixed_camera"
folder <- "data/output/fishing_effort/cloudpoint/fixed_camera/"


csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
df_list <- list()  

for (file in csv_files) {
  df <- read.csv(file)
  df_list[[file]] <- df
}
df_final <- do.call(rbind, df_list)  # Combine all dataframes in a single one

#Write the final stack .csv
#Change the path to output directory for stack .csv
#better use a relative path
write.csv(df_final, file = paste0("data/output/fishing_effort/cloudpoint/",method,"_fishing_effort.csv"), row.names = FALSE)

# select method to process ----------
# sat 
sat <- df_final
# ais
ais <- df_final
# fixed camera
fc <- df_final





# K1) AIS data ----------------------------------------------------------------
ais <- read.csv("data/output/fishing_effort/cloudpoint/ais_fishing_effort.csv")
# extrac day for filtering:
ais$timestamp <- as.Date(ais$timestamp)
ais$day <- format(ais$timestamp, "%d")
ais$day <- as.numeric(ais$day)

# create open or close fishing by the day
# Note: in case filter different range of open 1-20 -> na.omit
ais <- ais %>%
  mutate(fishing = case_when(
    day >= 1 & day <= 6 ~ "open",
    day >= 25 & day <= 31 ~ "close"))

ais <- ais %>% filter(!is.na(fishing))

# convert to sf from df / st
ais$lon <- ais$longitude
ais$lat <- ais$latitude
ais <- st_as_sf(ais, coords = c("longitude", "latitude"), crs = st_crs(mpa))


# plot 
k1 <- basemap + 
  # vessels points
  geom_point(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "lightblue", size = 3, alpha = 0.05) +
  geom_point(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey10", size = 2, alpha = 0.85) +
  geom_point(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#87CEFA", size = 1, alpha = 0.5) +
  # kernel
  stat_density_2d(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = after_stat(level)), 
                  geom = "polygon", 
                  contour = TRUE, 
                  colour = "grey20", 
                  alpha = 0.4, 
                  bins = 4) + 
  # color palette
  scale_fill_distiller(palette = "Blues", direction = 1) +
  # adjust densities data in x
  scale_x_continuous(limits = c(2.70, 2.76)) +
  scale_y_continuous(limits = c(39.40, 39.48)) +
  
  # face wrap / grid
  facet_wrap(vars(fishing), ncol = 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

k1





# K2) Satellite data -----------------------------------------------------------
sat <- read.csv("data/output/fishing_effort/cloudpoint/satellite_fishing_effort.csv")
# extrac day for filtering:
sat$acquired <- as.Date(sat$acquired)
sat$day <- format(sat$acquired, "%d")
sat$day <- as.numeric(sat$day)

# create open or close fishing by the day
# Note: in case filter different range of open 1-20 -> na.omit
sat <- sat %>%
  mutate(fishing = case_when(
    day >= 1 & day <= 6 ~ "open",
    day >= 25 & day <= 31 ~ "close"))

sat <- na.omit(sat)

# conver to sf from df / st
sat$lon <- sat$longitude
sat$lat <- sat$latitude
sat <- st_as_sf(sat, coords = c("longitude", "latitude"), crs = st_crs(mpa))

# plot
k2 <- basemap + 
  # vessels points
  geom_point(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#FFA54F", size = 3, alpha = 0.05) +
  geom_point(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey10", size = 2, alpha = 0.85) +
  geom_point(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "orange", size = 1, alpha = 0.5) +
  # kernel
  stat_density_2d(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = after_stat(level)), 
                  geom = "polygon", 
                  contour = TRUE, 
                  colour = "grey20", 
                  alpha = 0.4, 
                  bins = 10) + 
  # color palette
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  # adjust densities data in x
  scale_x_continuous(limits = c(min(sat$lon), 2.75)) +
  scale_y_continuous(limits = c(39.40, max(sat$lat))) +
  
  # face wrap / grid
  facet_wrap(vars(fishing), ncol = 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

k2



fc <- sat


# K3) Fixed camera data --------------------------------------------------------
# extrac day for filtering:
fc$acquired <- as.Date(fc$acquired)
fc$day <- format(fc$acquired, "%d")
fc$day <- as.numeric(fc$day)

# create open or close fishing by the day
# Note: in case filter different range of open 1-20 -> na.omit
fc <- fc %>%
  mutate(fishing = case_when(
    day >= 1 & day <= 6 ~ "open",
    day >= 25 & day <= 31 ~ "close"))

fc <- na.omit(fc)

# conver to sf from df / st
fc$lon <- fc$longitude
fc$lat <- fc$latitude
fc <- st_as_sf(fc, coords = c("longitude", "latitude"), crs = st_crs(mpa))

# plot
k3 <- basemap + 
  # vessels points
  geom_point(data = fc, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#9ACD32", size = 3, alpha = 0.05) +
  geom_point(data = fc, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey10", size = 2, alpha = 0.85) +
  geom_point(data = fc, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#A9E037", size = 1, alpha = 0.5) +
  # kernel
  stat_density_2d(data = fc, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = after_stat(level)), 
                  geom = "polygon", 
                  contour = TRUE, 
                  colour = "grey20", 
                  alpha = 0.4, 
                  bins = 10) + 
  # color palette
  scale_fill_distiller(palette = "Greens", direction = 1) +
  # adjust densities data in x
  scale_x_continuous(limits = c(min(fc$lon), 2.75)) +
  scale_y_continuous(limits = c(39.40, max(fc$lat))) +
  
  # face wrap / grid
  facet_wrap(vars(fishing), ncol = 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())
  
k3



# combine -----------------------------------------------
p <- grid.arrange(k1, k2, k3, ncol = 3)
p


# export / save plot
p_png <- "fig/fig5.png"
p_svg <- "fig/fig5.svg"
ggsave(p_png, p, width=31, height=17, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=31, height=17, units="cm", dpi=350, bg="white")
