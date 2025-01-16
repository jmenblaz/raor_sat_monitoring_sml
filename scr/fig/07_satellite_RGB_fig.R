# -----------------------------------------------------------------------------
# 07 - Plot RGB Planet satellite image 
# -----------------------------------------------------------------------------

# Plot satellite img before and after the open of the seassonal fishing ban

# Final image version (zoomed) created in QGIS

library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(prismatic)
library(gridExtra)


# load fishing area
fa <- read_sf("data/gis/fa/fishing_area.gpkg")

# load stack to gather differents raster bands
b <- stack("data/gis/satellite_img_fig/20180830.tif")
d <- stack("data/gis/satellite_img_fig/20180901.tif")
b <- as.data.frame(b, xy = TRUE)
d <- as.data.frame(d, xy = TRUE)
head(b)
head(d)

#Rename bands
b <- b %>% rename(Red   = X20180830_1,   
                  Green = X20180830_2,
                  Blue  = X20180830_3)
b <- b %>% filter(Red != 0)        

d <- d %>% rename(Red   = X20180901_1,   
                  Green = X20180901_2,
                  Blue  = X20180901_3)
d <- d %>% filter(Red != 0)  


# check RGB colors
# Specify Bands in the layer
prismatic::color(rgb(r = b$Red,              
                     g = b$Green,
                     b = b$Blue,
                     maxColorValue = 255)[1:200]) #subsample

prismatic::color(rgb(r = d$Red,              
                     g = d$Green,
                     b = d$Blue,
                     maxColorValue = 255)[1:200]) #subsample



# plots
# study area extension
sa <- read_sf("data/gis/study_area/study_area_raor.shp")
sa_extent <- st_bbox(sa)
# # create xl and yl object for ggplot
# xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) 
# yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0))

# For lastest image version (zoomed)
xl <- c((sa_extent["xmax"] - 0.007), (sa_extent["xmin"] + 0.012)) 
yl <- c((sa_extent["ymax"] - 0.017), (sa_extent["ymin"] + 0.0004))

# add shadow for no-fishing area
no_fa <- st_sym_difference(st_geometry(sa), st_geometry(fa))


# b, before
pb <- ggplot() +
  # satellite image
  geom_raster(data = b, aes(x = x, y = y, fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), 
              show.legend = FALSE) +
  scale_fill_identity() +
  # fishing area
  # geom_sf(data = fa, fill = "tan1", colour = "darkorange2", size = 1, alpha = 0.02) +
  # geom_sf(data = fa, fill = "NA", colour = "darkorange2", size = 1, alpha = 1) +
  # plot no fishing area
  geom_sf(data = no_fa, fill = "white", colour = "transparent", size = 1, alpha = 0) +
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
        panel.background = element_blank(),
        panel.grid = element_blank())
# pb


# after
pd <- ggplot() +
  # satellite image
  geom_raster(data = d, aes(x = x, y = y, fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), 
              show.legend = FALSE) +
  scale_fill_identity() +
  # fishing area
  # geom_sf(data = fa, fill = "tan1", colour = "darkorange2", size = 1, alpha = 0.02) +
  # geom_sf(data = fa, fill = "NA", colour = "darkorange2", size = 1, alpha = 1) +
  # plot no fishing area
  geom_sf(data = no_fa, fill = "white", colour = "transparent", size = 1, alpha = 0) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = F) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10, colour = "transparent"),
        axis.text.x = element_text(size = 10),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank())
# pd


# combine
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
p <- grid.arrange(pb, pd, ncol = 2)
p


# export / save plot
p_png <- "fig/fig_sat_zoomed.png"
p_svg <- "fig/fig_sat_zoomed.svg"
ggsave(p_png, p, width=25, height=12, units="cm", dpi=400, bg="white")
ggsave(p_svg, p, width=25, height=12, units="cm", dpi=400, bg="white")

