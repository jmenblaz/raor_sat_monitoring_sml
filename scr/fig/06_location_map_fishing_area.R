# --------------------------------------------------------------
# 06 - Location map and fishinf area
# --------------------------------------------------------


# Plot 1 - Global location
# Plot 2 - Balearic archipielago
# Plot 3 - Study area

# Plot 4 - Supplementary material fishing area


library(sf)
library(dplyr)
library(gghalves)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggtext)
library(ggpattern)
library(data.table)





# land, study area (sa), MPA, non-take area and buffer 200 from coastline
sa   <- read_sf("data/gis/study_area/study_area_raor.shp")
land <- read_sf("data/gis/admin_balearic_islands/illesBaleares.shp")
mpa  <- read_sf("data/gis/mpa/mpapb.gpkg")
notake_area <- read_sf("data/gis/mpa/mpapb_notake_area.gpkg")
b200 <- read_sf("data/gis/admin_balearic_islands/buffer200_landsmask.gpkg")
posidonia <- read_sf("data/gis/posidonia/posidonia_raor.gpkg")

# assign same CRS to data_sf
st_crs(posidonia) <- st_crs(notake_area)







# Plot 1 - World Map
library(pacman)
pacman::p_load(dplyr, data.table, rnaturalearth, rnaturalearthdata, 
               ggplot2, raster, terra, tidyr, stringr, gridExtra, 
               plotly, sf, ggshadow, ggforce, giscoR, install = TRUE)

# Custom global orthographic proyection from arctic region
ortho_crs <-'+proj=ortho +lat_0=35 +lon_0=0 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

# world coastlines from giscoR package
world_poly <- gisco_get_countries(year = "2016", epsg = "4326", resolution = "10")

# global graticule
grid <- st_graticule()

# ocean mask 
ocean <- st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # Planet earth radius (m)
  st_sfc(crs = ortho_crs)

# Select visible area and project
world <- world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) # 
# plot(world)

# delete grid trough continents to create a clean grid
grid_crp <- st_difference(grid, st_union(world_poly))

# select visible area
grid_crp <- st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)
# plot(grid_crp)

# cover globe limit into df - datframe
ocean_df <- st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

# build shadow 
ggplot() + 
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 1.5) +
  coord_sf() +
  theme_void()

# add more shadows
g <- ggplot() +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.06,
                shadowsize = 1.5) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.02,
                shadowsize = 1) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = .5)

# add grid
g2 <- g + 
      # ocean
      geom_sf(data = ocean,
              fill = "grey37",
              alpha = 0.9) +
      # grid
      geom_sf(data = grid_crp, 
        colour = "grey60",
        alpha = 0.5,
        linewidth = .15) +
      # add 3D globe land
      geom_sf(data = world, 
          colour = "grey40",
          fill = "grey20",
          linewidth = .10) +
        # theme
        theme_void() +
        theme(panel.background = element_blank())
g2

# export / save plot
p_png <- "fig/fig1_1.png"
p_svg <- "fig/fig1_1.svg"
ggsave(p_png, g2, width=10, height=10, units="cm", dpi=350)
ggsave(p_svg, g2, width=10, height=10, units="cm", dpi=350)




# Plot 2 - Balearic archipielago --------------------------------------
land <- read_sf("data/gis/admin_balearic_islands")
xl <- c(1.1,4.4)
yl <- c(38.55, 40.2)


g3 <- ggplot() +
  # land mask
  geom_sf(data = land, fill = "grey25", colour = "black", size = 10, alpha = 0.95) +
  # study area
  geom_sf(data = sa, fill = "orange", colour = "grey10", size = 10, alpha = 0.7) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = F) +
  # y axi
  scale_y_continuous(breaks = c(38.8,39.2,39.6,40.0)) + 
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 7),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(5, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_rect(fill = "grey80"),
        panel.grid = element_blank())

g3

# export / save plot
p_png <- "fig/fig1_2.png"
p_svg <- "fig/fig1_2.svg"
ggsave(p_png, g3, width= 10, height=7.5, units="cm", dpi=350, bg = "white")
ggsave(p_svg, g3, width= 10, height=7.5, units="cm", dpi=350, bg = "white")






# Plot 3 - Study area -------------------------------------------------------

# study area extent
sa_extent <- st_bbox(sa)

# create xl and yl object for ggplot
xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) 
yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0))


basemap <- ggplot() +
  # MPA
  geom_sf(data = mpa, fill = "grey80", colour = "#000000", size = 15, linetype = "dashed", alpha = 0.75) +
  # Posidonia cover
  geom_sf(data = posidonia, fill = "#55711c", colour = "grey15", size = 1, alpha = 0.45) +
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
        panel.background = element_rect(fill = "grey80"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))


basemap


# export / save plot
# export / save plot
p_png <- "fig/fig1_3.png"
p_svg <- "fig/fig1_3.svg"
ggsave(p_png, basemap, width=10, height=12, units="cm", dpi=350, bg="white")
ggsave(p_svg, basemap, width=10, height=12, units="cm", dpi=350, bg="white")





# Plot 4 - Fishing Area (Supplemenary Material) --------------------------------

fa <- read_sf("data/gis/fa/fishing_area.gpkg")

st_crs(fa) <- st_crs(notake_area)
st_area(fa) # 7.693304 km2

basemap <- ggplot() +
  # No-take area
  geom_sf(data = notake_area, fill = "#CD5B45", colour = "grey10", size = 9, alpha = 0.55) +
  # land mask
  geom_sf(data = land, fill = "grey25", colour = "black", size = 10, alpha = 0.95) +
  # fishing area
  geom_sf_pattern(data = fa, fill = "orange", colour = "#000000", size = 0.75, alpha = 0.85,
                  pattern = "stripe") +
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
        panel.background = element_rect(fill = "grey80"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))


basemap


# export / save plot
# export / save plot
p_png <- "fig/sup_fig_fa.png"
p_svg <- "fig/sup_fig_fa.svg"
ggsave(p_png, basemap, width=12, height=16, units="cm", dpi=350, bg="white")
ggsave(p_svg, basemap, width=12, height=16, units="cm", dpi=350, bg="white")
