#------------------------------------------------------------------------------
# 02. Plot - Difference in AIS and digitized ships
#--------------------------------------------------------------------------


# 1) Raincloud to visualize differences in the length of the boats of AIS and digitalized 
#    (potential Supplementary Material)

# 2) Cloudpoints of the points spatial distribtion (Supplementary Material)

library(sf)
library(dplyr)
library(gghalves)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggtext)
library(data.table)

library(raster)

#devtools::install_github('smin95/smplot2', force = TRUE)
library(smplot2)



# 1) Raincloud to visualize differences in the length of the boats of AIS and digitized -----
#    (potential Supplementary Material)

# load data of AIS and satellite pairs of ships detected
pairs <- read.csv("data/output/ais/length_ais_sat_pairs.csv")


# prepare data for plot

ais_df <- data.frame(length = pairs$ais_length,
                     method = "ais")

ship_df <- data.frame(length = pairs$sat_length,
                      method = "satellite")

# combine dfs
df <- rbind(ais_df, ship_df)

df$method <- as.factor(df$method)

# prepare data
df_ais <- filter(df, method == 'ais')
df_sat <- filter(df, method == 'satellite')


# color palette
sm_palette(20)

# raincloud plot
p <- ggplot(data = df, mapping = aes(x = method, y = length, fill = method)) +
  
  sm_raincloud(data = df_ais, position = position_nudge(x = -0.05),
               boxplot.params = list(outlier.shape = NA),
               show.legend = FALSE,
               vertical = TRUE,
               which_side = 'l',
               point.params = list(size = 1.4, shape = 21,
                                   colour = "skyblue", 
                                   show.legend = FALSE,
                                   alpha = 0.35,
                                   position = sdamr::position_jitternudge(nudge.x = +0.16, seed = 15,
                                                                          jitter.width = 0.20))) +
  
  sm_raincloud(data = df_sat, position = position_nudge(x = +0.05),
               boxplot.params = list(outlier.shape = NA),
               show.legend = FALSE,
               vertical = TRUE,
               point.params = list(size = 1.4, shape = 21,
                                   colour = "orange", 
                                   show.legend = FALSE,
                                   alpha = 0.35,
                                   position = sdamr::position_jitternudge(nudge.x = -0.16, seed = 15,
                                                                          jitter.width = 0.25))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # labels groups
  scale_x_discrete(labels = c('AIS', 'Satellite')) +
  #
  scale_y_continuous(limits = c(5,60), breaks = c(10,20,30,40,50,60))  +
  # y lab
  ylab('Ship lenght (m)') +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y  = element_text(size = 7.5),
        axis.text.x  = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA, size = 1.1))

p

# save plot
p_png <- "fig/fig2_shiplenght_ais_sat.png"
p_svg <- "fig/fig2_shiplenght_ais_sat.svg"
ggsave(p_png, p, width=7, height=8, units="cm", dpi=450, bg="white")
ggsave(p_svg, p, width=7, height=8, units="cm", dpi=450, bg="white")









# 2) Maps composition of ships detected cloudpoints ---------------------------

# load ship, vessel and camera data
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
ais_f <- read.csv("data/output/fishing_effort/cloudpoint/ais_fishing_effort.csv")

# ship digitized
ships <- read.csv("data/output/shipProc.csv", sep = ";")
ships <- ships %>% filter(duplicated == "FALSE") # filter ships duplicated

ships_f <- read.csv("data/output/fishing_effort/cloudpoint/satellite_fishing_effort.csv", sep = ",")
ships_f <- ships_f %>% filter(duplicated == "FALSE") # filter ships duplicated

# convert to sf
ais <- st_as_sf(ais, coords = c("longitude", "latitude"))
ais_f <- st_as_sf(ais_f, coords = c("longitude", "latitude"))

ships <- st_as_sf(ships, coords = c("longitude", "latitude"))
ships_f <- st_as_sf(ships_f, coords = c("longitude", "latitude"))

# land, study area (sa), MPA, fishing area, non-take area and buffer 200 from coastline
sa   <- read_sf("data/gis/study_area/study_area_raor.shp")
land <- read_sf("data/gis/admin_balearic_islands/illesBaleares.shp")
mpa  <- read_sf("data/gis/mpa/mpapb.gpkg")
notake_area <- read_sf("data/gis/mpa/mpapb_notake_area.gpkg")
b200 <- read_sf("data/gis/admin_balearic_islands/buffer200_landsmask.gpkg")
posidonia <- read_sf("data/gis/posidonia/posidonia_raor.gpkg")
fa <- read_sf("data/gis/fa/fishing_area.gpkg")

# assign same CRS to data_sf
st_crs(ais)  <- st_crs(notake_area)
st_crs(ais_f) <- st_crs(notake_area)
st_crs(ships)  <- st_crs(notake_area)
st_crs(ships_f) <- st_crs(notake_area)
st_crs(posidonia) <- st_crs(notake_area)

# study area extent
sa_extent <- st_bbox(sa)

# create xl and yl object for ggplot
xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) 
yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0))


# info plots dataset
nais  <- nrow(ais)
nship <- nrow(ships)



# 2.0) Basemap 
basemap <- ggplot() +
  # MPA
  # geom_sf(data = mpa, fill = "grey50", colour = "black", size = 12, linetype = "dashed", alpha = 0.9) +
  # No-take area
  geom_sf(data = notake_area, fill = "#CD5B45", colour = "grey10", size = 9, alpha = 0.75) +
  # fishing area
  geom_sf(data = fa, fill = "tan1", colour = "grey5", size = 2, alpha = 0.25) +
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
        panel.background = element_rect(fill = "grey65"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(38, "pt"),
        legend.key.height = unit(11, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

basemap






# 2.1) Plot cloudpoints of different methods -----------------------------------

# P1 - Cloudpoint AIS 
p1 <- basemap + 
  
  # ais vessels points
  geom_point(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "lightblue", size = 3.0, alpha = 0.05) +
  geom_point(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey15", size = 2.3, alpha = 0.75) +
  geom_point(data = ais, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "deepskyblue3", size = 1, alpha = 0.60) + 
  
  # ais vessels points (fishing area)
  geom_point(data = ais_f, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "lightblue", size = 3.6, alpha = 0.15) +
  geom_point(data = ais_f, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey15", size = 2.9, alpha = 1) +
  geom_point(data = ais_f, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#87CEFA", size = 1.7, alpha = 0.90)  
  
# plot info
#  annotate("text", x = 2.7495, y = 39.4635, label = "AIS", parse = TRUE, family = "Arial",
#           size = 4, color = "snow1") +
#  annotate("text", x = 2.748, y = 39.4615, label = paste0("italic(n) == ", nais), parse = TRUE, family = "Arial",
#           size = 3.5, color = "snow1") +
#  annotate("pointrange", x = 2.746, y = 39.4635, ymin = 12, ymax = 28,
#           colour = "lightblue", size = 1.3, linewidth = 0.75, alpha = 0.2) +
#  annotate("pointrange", x = 2.746, y = 39.4635, ymin = 12, ymax = 28,
#           colour = "#000000", size = 1.1, linewidth = 0.75) +
#  annotate("pointrange", x = 2.746, y = 39.4635, ymin = 12, ymax = 28,
#           colour = "#87CEFA", size = 0.8, linewidth = 0.75)
  
p1


# P2 - Total Cloudpoint Satellite 
p2 <- basemap +
  # vessels points
  geom_point(data = ships, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#FFA54F", size = 2.7, alpha = 0.05) +
  geom_point(data = ships, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey15", size = 1.8, alpha = 0.5) +
  geom_point(data = ships, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "darkorange2", size = 0.75, alpha = 0.45) +
  
  # vessel points (fishing area)
  geom_point(data = ships_f, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "#FFA54F", size = 3.6, alpha = 0.1) +
  geom_point(data = ships_f, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey15", size = 2.9, alpha = 0.8) +
  geom_point(data = ships_f, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "orange", size = 1.7, alpha = 0.8) 
  
  # plot info
#  annotate("text", x = 2.7478, y = 39.4635, label = "Satellite", parse = TRUE, family = "Arial",
#           size = 4, color = "snow1") +
#  annotate("text", x = 2.748, y = 39.4615, label = paste0("italic(n) == ", nship), parse = TRUE, family = "Arial",
#           size = 3.5, color = "snow1") +
#  annotate("pointrange", x = 2.742, y = 39.4635, ymin = 12, ymax = 28,
#           colour = "#FFA54F", size = 1.3, linewidth = 0.75, alpha = 0.2) +
#  annotate("pointrange", x = 2.742, y = 39.4635, ymin = 12, ymax = 28,
#           colour = "#000000", size = 1.1, linewidth = 0.75) +
#  annotate("pointrange", x = 2.742, y = 39.4635, ymin = 12, ymax = 28,
#           colour = "orange", size = 0.8, linewidth = 0.75)

p2


# Combine
p <- grid.arrange(p1, p2, ncol = 2)
p


p_png <- "fig/fig3_ships.png"
p_svg <- "fig/fig3_ships.svg"
ggsave(p_png, p, width = 20, height = 12, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width = 20, height = 12, units="cm", dpi=350, bg="white")












