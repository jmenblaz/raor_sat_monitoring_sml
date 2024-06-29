
#------------------------------------------------------------------------------
# 1. Descriptive spatial statistics 
#------------------------------------------------------------------------------

library(sf)
library(fs)
library(rgdal)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)
library(gridExtra)
# For giff
library(gganimate)
library(purrr)
library(magick)

## paths
# output directory
outdir <- "data/output/"

# 0. load vessel dataset created
data <- read.csv("data/output/shipProc.csv")

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



# Descriptive statistics ------------------------------------------------------

# Convertir la columna 'acquired' a formato de fecha
data$acquired <- as.POSIXct(data$acquired)

# Extraer el año de cada fecha
data$year <- year(data$acquired)

# Contar el número de registros por día sin tener en cuenta el año
registros_por_dia <- data %>%
  mutate(dia = format(as.Date(acquired), "%m-%d")) %>%
  group_by(dia, year) %>%
  summarise(num_registros = n())

# daily mean
media_por_dia <- registros_por_dia %>%
  group_by(dia) %>%
  summarise(media_registros = median(num_registros))


# plot
ggplot(data = registros_por_dia, aes(x = dia, y = num_registros, group = year, color = factor(year))) +
  geom_line(size = 0.5, alpha = 0.4) +
  # geom_smooth(method = "loess", span = 0.5, se = FALSE) + # smooth lines
  stat_summary(aes(y = num_registros, group = 1), fun.y = median, colour = "grey10", geom = "smooth", size = 1 )  +
  geom_point(size = 2.5) +
  labs(x = "Date", y = "Vessels", color = "Year") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 10))



# Descriptive spatial Analysis ----------------------------------------------- 

# Basemap ------------------------
# Map land and study area (sa)
sa <- read_sf("data/gis/study_area/study_area_raor.shp")
land <- read_sf("data/gis/admin_balearic_islands/illesBaleares.shp")

# study area extent
sa_extent <- st_bbox(sa)

# create xl and yl object for ggplot
xl <- c((sa_extent["xmax"] + 0.0), (sa_extent["xmin"] - 0.0)) # +- 0.51 and 0.50 is to adjust de extension of map to box of plot
yl <- c((sa_extent["ymax"] + 0.0), (sa_extent["ymin"] - 0.0)) # +- 0.40 and 0.38 is to adjust de extension of map to box of plot



# 1. Plot total cloud-point digitized ------
p1 <- ggplot() +
      # land mask
      geom_sf(data = land, fill = "white", colour = "black", size = 3) +
      # study area
      geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
      # spatial extension
      coord_sf(xlim = xl, ylim = yl, expand = TRUE) +
      # vessels points
      geom_point(data = data, aes(x = longitude, y = latitude), color = "skyblue4", size = 2, alpha = 0.4) +
      # label scale
      # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
      # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
      # labels
      # Theme
      theme_bw() +
      theme(axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 11),
            axis.ticks = element_line(size = 1),
            axis.ticks.length = unit(7, "pt"),
            axis.title = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, size = 1.2),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.justification = "center",
            legend.key.width = unit(40, "pt"),
            legend.key.height = unit(12, "pt"),
            legend.text = element_text(size = 12))

print(p1)




# 2. Kernel density map of total of total cloudpoint -----
p2 <- ggplot() +
  # land mask
  geom_sf(data = land, fill = "white", colour = "black", size = 5) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = TRUE) +
  # vessels points
  geom_point(data = data, aes(x = longitude, y = latitude), color = "grey40", size = 0.5, alpha = 0.2) +
  # Kernel
  geom_density_2d(data = data, aes(x = longitude, y = latitude), colour = "grey20", alpha = 0.75) +
  geom_density_2d_filled(data = data, aes(x = longitude, y = latitude), alpha = 0.5) +
  # label scale
  # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
  # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
  # labels
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(12, "pt"),
        legend.key.height = unit(12, "pt"),
        legend.text = element_text(size = 10))

print(p2)

# Plot composition
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)










# -----------------------------------------------------------------------------
# 2) Data outside No-take area in Palma Bay Marine Protected Area
# -----------------------------------------------------------------------------

# Load no-take area
notake_area <- read_sf("data/gis/mpa/mpapb_notake_area.gpkg")

# convert to sf
data_out <- st_as_sf(data, coords = c("longitude", "latitude"))
# assign same CRS to data_sf
st_crs(data_out) <- st_crs(notake_area)

# Data spatial filtering for select only points outside notake_area
# use spatial difference to select points of interest
data_out <- st_difference(data_out, notake_area)


# Transfrom into same CRS
data_out <- st_transform(data_out, st_crs(sa))

# 1. Plot total cloud-point digitized outside no-take area -------------
p1 <- ggplot() +
  # land mask
  geom_sf(data = land, fill = "white", colour = "black", size = 3) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
  # No-take area
  geom_sf(data = notake_area, fill = "grey80", colour = "grey15", size = 8) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = TRUE) +
  # vessels points
  geom_point(data = data_out, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "skyblue4", size = 2, alpha = 0.4) +
  
  # Note: problem to plotting geom_sf direclty -> spatial extension did no work..
  # label scale
  # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
  # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
  
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(40, "pt"),
        legend.key.height = unit(12, "pt"),
        legend.text = element_text(size = 12))

print(p1)





# 2. Kernel density map of total of  cloudpoint outside no-take area -----
p2 <- ggplot() +
  # land mask
  geom_sf(data = land, fill = "white", colour = "black", size = 5) +
  # study area
  geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
  # No-take area
  geom_sf(data = notake_area, fill = "grey80", colour = "grey15", size = 8) +
  # spatial extension
  coord_sf(xlim = xl, ylim = yl, expand = TRUE) +
  # vessels points
  geom_point(data = data_out, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
             color = "grey40", size = 0.5, alpha = 0.2) +
  # kernel
  geom_density_2d(data = data_out, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])
                  , colour = "grey20", alpha = 0.75) +
  geom_density_2d_filled(data = data_out, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])
                         , alpha = 0.5) +
  # label scale
  # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
  # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(7, "pt"),
        axis.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(12, "pt"),
        legend.key.height = unit(12, "pt"),
        legend.text = element_text(size = 10))

print(p2)

# Plot composition
grid.arrange(p1, p2, ncol = 2)








# 3. Daily kernel density map of cloudpoint outside no-take area ------------

# extract days %m-%d from "acquired" info into a new column
data_out$day <- format(data_out$acquired, "%m-%d")

# unique days with satellite image
days <- unique(data_out$day)
# order 
days <- days[order(days)]  #Note: ordered as a chain of characters, not dates


for (d in days) {

    # filter by day
    data_out_plot <- data_out %>%
      subset(day == d)
    
    # plot
    p_day <- ggplot() +
      # land mask
      geom_sf(data = land, fill = "white", colour = "black", size = 5) +
      # study area
      geom_sf(data = sa, fill = NA, colour = "grey25", size = 6) +
      # No-take area
      geom_sf(data = notake_area, fill = "grey80", colour = "grey15", size = 8) +
      # spatial extension
      coord_sf(xlim = xl, ylim = yl, expand = TRUE) +
      # vessels points
      geom_point(data = data_out_plot, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]),
                 color = "grey40", size = 0.5, alpha = 0.2) +
      # kernel
      geom_density_2d(data = data_out_plot, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])
                      , colour = "grey20", size = 0.2, alpha = 0.75) +
      geom_density_2d_filled(data = data_out_plot, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])
                             , alpha = 0.5) +
      ggtitle(d) +
      # label scale
      # scale_y_continuous(breaks = seq(36, 42, length.out = 4)) +
      # scale_x_continuous(breaks = seq(0, 8, length.out = 3)) +
      # Theme
      theme_bw() +
      theme(axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            axis.ticks = element_line(size = 0.4),
            axis.ticks.length = unit(6, "pt"),
            axis.title = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, size = 1),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.position = "none",
            legend.direction = "vertical",
            legend.justification = "center",
            legend.key.width = unit(12, "pt"),
            legend.key.height = unit(12, "pt"),
            legend.text = element_text(size = 10))
    
    # print(p_day)
    #save
    ggsave(paste0("data/output/plots/","plot_", d, ".jpeg"), plot = p_day, device = "jpeg")
}



# Giff 
# list plots created
img <- fs::dir_ls('data/output/plots')
# rad each image in the list "img" created -> image_read() in "magick" package
img <- purrr::map(img, image_read)
# join img
jnd <- image_join(img)
anm <- magick::image_animate(jnd, fps = 1)
# write
image_write(image = anm, path = "data/output/plots/giff/plots.gif")


