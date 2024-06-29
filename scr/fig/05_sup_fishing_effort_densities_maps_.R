# -----------------------------------------------------------------------------
# 5) Fishing effort - Supplementary Material ----------------------------------

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
st_crs(posidonia) <- st_crs(notake_area)

# study area extent
sa_extent <- st_bbox(sa)

# create xl and yl object for ggplot
xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) 
yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0))


basemap <- ggplot() +
  # posidonia
  # geom_sf(data = posidonia, fill = "#55711c", colour = "grey10", size = 1, alpha = 0.35) +
  geom_sf(data = posidonia, fill = "grey35", colour = "grey10", size = 1, alpha = 0.35) +
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
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7, angle = -90, hjust = -0.08, vjust = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(4, "pt"),
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
    day >= 1 & day <= 20 ~ "open",
    day >= 25 & day <= 31 ~ "close"))

ais <- ais %>% filter(!is.na(fishing))


# conver to sequential order 1-25, 2-26
# logic:
# Si order es 25 o mayor, se le resta 24 para que empiece desde 1
# Si order es menor a 25, se le suma 7 para continuar la secuencia.
ais$order <- as.numeric(ais$day)
ais <- ais %>%
  mutate(order = ifelse(day >= 25, order- 24, order + 7))

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
                  alpha = 0.45, 
                  bins = 5) + 
  # color palette
  scale_fill_distiller(palette = "Blues", direction = 1) +
  # adjust densities data in x
  scale_x_continuous(limits = c(2.70, 2.76)) +
  scale_y_continuous(limits = c(39.40, 39.48)) +
  
  # face wrap / grid
  facet_wrap(vars(order), ncol = 5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

k1

warnig# export / save plot
p_png <- "fig/sup_fig21.png"
p_svg <- "fig/sup_fig21.svg"
ggsave(p_png, k2, width=22, height=25, units="cm", dpi=350, bg="white")
ggsave(p_svg, k2, width=22, height=25, units="cm", dpi=350, bg="white")



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
    day >= 1 & day <= 20 ~ "open",
    day >= 25 & day <= 31 ~ "close"))

sat$order <- as.numeric(sat$day)
sat <- sat %>%
  mutate(order = ifelse(day >= 25, order- 24, order + 7))


# conver to sf from df / st
sat$lon <- sat$longitude
sat$lat <- sat$latitude
sat <- st_as_sf(sat, coords = c("longitude", "latitude"), crs = st_crs(mpa))

# plot
k2 <- basemap + 
  # vessels points
  geom_point(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#FFA54F", size = 2.5, alpha = 0.05) +
  geom_point(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey10", size = 1.75, alpha = 0.85) +
  geom_point(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "orange", size = 0.75, alpha = 0.5) +
  # kernel
  stat_density_2d(data = sat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], fill = after_stat(level)), 
                  geom = "polygon", 
                  contour = TRUE, 
                  colour = "grey10", 
                  alpha = 0.6, 
                  bins = 10) + 
  # color palette
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  # adjust densities data in x
  scale_x_continuous(limits = c(2.69, 2.75)) +
  scale_y_continuous(limits = c(39.40, 39.50)) +
  
  # face wrap / grid
  facet_wrap(vars(order), ncol = 5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

k2


# export / save plot
p_png <- "fig/sup_fig22.png"
p_svg <- "fig/sup_fig22.svg"
ggsave(p_png, k2, width=22, height=25, units="cm", dpi=350, bg="white")
ggsave(p_svg, k2, width=22, height=25, units="cm", dpi=350, bg="white")







fc <- sat


# K3) Fixed camera data --------------------------------------------------------
# extract day for filtering:
fc$acquired <- as.Date(fc$acquired)
fc$day <- format(fc$acquired, "%d")
fc$day <- as.numeric(fc$day)

# create open or close fishing by the day
# Note: in case filter different range of open 1-20 -> na.omit
fc <- fc %>%
  mutate(fishing = case_when(
    day >= 1 & day <= 20 ~ "open",
    day >= 25 & day <= 31 ~ "close"))

fc <- na.omit(fc)

# sequential seq based on day
fc$order <- as.numeric(fc$day)
fc <- fc %>%
  mutate(order = ifelse(day >= 25, order- 24, order + 7))

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
                  alpha = 0.6, 
                  bins = 10) + 
  # color palette
  scale_fill_distiller(palette = "Greens", direction = 1) +
  # adjust densities data in x
  scale_x_continuous(limits = c(min(fc$lon), 2.75)) +
  scale_y_continuous(limits = c(39.40, max(fc$lat))) +
  
  # face wrap / grid
  facet_wrap(vars(order), ncol = 5) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

k3





# export / save plot
p_png <- "fig/sup_fig23.png"
p_svg <- "fig/sup_fig23.svg"
ggsave(p_png, k3, width=22, height=25, units="cm", dpi=350, bg="white")
ggsave(p_svg, k3, width=22, height=25, units="cm", dpi=350, bg="white")
