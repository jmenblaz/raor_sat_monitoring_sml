#------------------------------------------------------------------------------
# 01. Plot coverage satellite area and digitalized area
#------------------------------------------------------------------------------


# Suplementary material 2

#   2.1 Scene coverage in study area
#   2.2 Digitalized effort (scenes/week)
#   2.3 Cloudcover


library(sf)
library(ggplot2)
library(raster)
library(rasterVis)
library(viridis)
library(gridExtra)

# Basemap layers --------------------------------------------------------------
# land and study area (sa)
sa <- read_sf("data/gis/study_area/study_area_raor.shp")
land <- read_sf("data/gis/admin_balearic_islands/illesBaleares.shp")

# study area extent
sa_extent <- st_bbox(sa)

# create xl and yl object for ggplot
xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) # +- 0.51 and 0.50 is to adjust de extension of map to box of plot
yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0)) # +- 0.40 and 0.38 is to adjust de extension of map to box of plot


# 2.1 Scene coverage in study area --------------------------------------------

# raster data
r <- raster("data/output/raster/01_num_scene_coverage.tif")
# convert rater to df and rename variables for plot raster
r <- as.data.frame(r, xy=TRUE)
colnames(r) <- c("x", "y", "value")

# coloRamp package 
colRamp <- colorRampPalette(c("#ffffff", "#ffe9d6", "#ffd3ae", "#ffbc85", "#ffa65c", "#CD6600"))(50)

p1 <- ggplot() +
  # scene coverage
  geom_raster(data = r, aes(x = x, y = y, fill = value)) +
  # coloRamp
  scale_fill_gradientn(colors = colRamp,
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       na.value = "#FFFFFF") +
  # land mask
  geom_sf(data = land, fill = "grey25", colour = "black", size = 3) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
  # label scale
  # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
  # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
  # labels
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
p1



# 2.2 Scene coverage in study area --------------------------------------------

# raster data
r <- raster("data/output/raster/05_digit_area_num_scene_week.tif")
# convert rater to df and rename variables for plot raster
r <- as.data.frame(r, xy=TRUE)
colnames(r) <- c("x", "y", "value")

# coloRamp package 
colRamp <- colorRampPalette(c("#ffffff", "#ffe9d6", "#ffd3ae", "#ffbc85", "#ffa65c", "#CD6600"))(50)

p2 <- ggplot() +
  # scene coverage
  geom_raster(data = r, aes(x = x, y = y, fill = value)) +
  # coloRamp
  scale_fill_gradientn(colors = colRamp,
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       na.value = "#FFFFFF") +
  # land mask
  geom_sf(data = land, fill = "grey25", colour = "black", size = 3) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
  # label scale
  # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
  # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
  # labels
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
p2



#   2.3 Cloudcover ------------------------------------------------------------

# raster
r <- raster("data/output/raster/07_mean_cloudcover_scenes.tif")
r <- as.data.frame(r, xy=TRUE)
colnames(r) <- c("x", "y", "value")

# coloRamp package 
colRamp <- colorRampPalette(c('#B9D3EE','#FFFAFA','#C7C7C7','#ABABAB','#8B8682','#404040'))(50)

p3 <- ggplot() +
  # scene coverage
  geom_raster(data = r, aes(x = x, y = y, fill = value)) +
  
  scale_fill_gradientn(colors = colRamp,
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                       na.value = "#FFFFFF",
  ) +
  
  # land mask
  geom_sf(data = land, fill = "grey25", colour = "black", size = 3) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
  # label scale
  # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
  # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
  # labels
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 9))
p3


# arrange plots
p <- grid.arrange(p1,p2,p3, ncol = 3)
p

# export / save plot
p_png <- "fig/sup_fig4.png"
p_svg <- "fig/sup_fig4.svg"
ggsave(p_png, p, width=32, height=16, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=32, height=16, units="cm", dpi=350, bg="white")



