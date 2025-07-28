

# 02 ais_range_time_broadcasted.

# by Javier Menéndez-Blázquez


# Check the average and range of time differences between 
# the last broadcasted AIS position and the satellite image time? 

# This script obtained the timestamp of the AIS position between +-20 min
# in order to calculate the range of time (also mean. median, and plots) between
# PlanetScope scenes

# see 02_AIS_scene_scene for main workflow.

# AIS: All AIS data per each day in +-0.05 degrees of study area or bounding box
# AIS: +- 20 min scene timeacquired



# 1) Process data 
# 2) Exploratory results, means timerange per scene
# 3) Sup figures in this same scripts



library(sf)
library(dplyr)
library(lubridate)
library(data.table)

# load data -------------------------------------------------------------------

# AIS (previously filtered for bbox of study area based on days of the unique timestamps scenes)
ais <- read.csv("data/ais/aisRaw.csv")
# remove column grt and dwt
ais <- subset(ais, select = -c(grt, dwt))
# rename columns lat and lon for move object
ais <- ais %>% 
  rename(latitude = lat,
         longitude = lon)

# scene info and spatial data
df_gpkg <- st_read("data/output/scene_data.gpkg")
# bbox study area
study_area <- st_read("data/gis/study_area/study_area_raor.gpkg")

# change timestamp format
ais$timestamp <- as.POSIXct(ais$timestamp)


results_list <- list() # to append differenrt results

t <- Sys.time()

for (l in 1:nrow(df_gpkg)) {
  # extract scene
  cat("Processing scene: ", l," / ",nrow(df_gpkg), "\n")
  scene <- df_gpkg[l, ]
  tstamp <- scene$acquired_UTC
  
  # scene extent
  ext <- st_bbox(scene)
  # filter AIS dataset in the study area by scene timestamp (+- 20 min)
  ais_scene <- ais |> filter(
    timestamp >= (tstamp-(20*60)) &
      timestamp <= (tstamp+(20*60)))
  
  # remove duplicated AIS messages
  ais_scene <- ais_scene %>%
    distinct(timestamp, mmsi, .keep_all = TRUE) %>%
    arrange(mmsi, timestamp)
  
  # Check point (no AIS in the scene after filtering --> next)
  if (nrow(ais_scene) == 0) next
  
  unique(ais_scene$mmsi)
  
  # For each vessel (mmsi), get the last position BEFORE the scene timestamp
  # ais_last_positions <- ais_scene %>%
  #   filter(timestamp <= tstamp) %>%
  #   group_by(mmsi) %>%
  #   filter(timestamp == max(timestamp)) %>%
  #   ungroup() %>%
  #   mutate(scene_timestamp = tstamp,
  #          time_diff_min = as.numeric(difftime(scene_timestamp, timestamp, units = "mins")),
  #          img_ID = scene$img_ID)
  
  
  # BEFORE and AFTER the scene timestamp.
  ais_last_positions <- ais_scene %>%
    group_by(mmsi) %>%
    slice_min(abs(as.numeric(difftime(timestamp, tstamp, units = "secs"))), n = 1) %>%
    ungroup() %>%
    mutate(
      scene_timestamp = tstamp,
      time_diff_min = abs(as.numeric(difftime(scene_timestamp, timestamp, units = "mins"))),
      time_diff_min_signed = as.numeric(difftime(timestamp, scene_timestamp, units = "mins")),
      img_ID = scene$img_ID
    )
  
  # append to list
  results_list[[l]] <- ais_last_positions

}

ais_timerange_scenes <- do.call(rbind, results_list)

Sys.time() - t  # ≈ 10 seconds



unique(ais_timerange_scenes$img_ID)

# Save / export results
write.csv(ais_timerange_scenes, "data/output/ais_timeranges_scenes.csv")







# ------------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# 2) Exploratory results and analysis
# --------------------------------------------------------------------

# read data
ais_timerange_scenes <- read.csv("data/output/ais_timeranges_scenes.csv")

# time range global
summary(ais_timerange_scenes$time_diff_min_signed)
summary(abs(ais_timerange_scenes$time_diff_min_signed))


# time differences between positios and scenes (global)
mean_diff <- mean(ais_timerange_scenes$time_diff_min_signed)
sd_diff <- sd(ais_timerange_scenes$time_diff_min_signed)
min_diff <- min(ais_timerange_scenes$time_diff_min_signed)
max_diff <- max(ais_timerange_scenes$time_diff_min_signed)

mean_abs_diff <- mean(abs(ais_timerange_scenes$time_diff_min_signed))

cat("Mean signed time difference:", mean_diff, "minutes\n")
cat("Mean absolute time difference:", mean_abs_diff, "minutes\n")
cat("SD of signed time difference:", sd_diff, "minutes\n")
cat("Range of signed time difference:", min_diff, "to", max_diff, "minutes\n")


table(sign(ais_timerange_scenes$time_diff_min_signed))
prop.table(table(sign(ais_timerange_scenes$time_diff_min_signed)))

prop.table(table(abs(ais_timerange_scenes$time_diff_min_signed) <= 5))
prop.table(table(abs(ais_timerange_scenes$time_diff_min_signed) <= 10))
prop.table(table(abs(ais_timerange_scenes$time_diff_min_signed) <= 15))



library(ggplot2)
library(gridExtra)

p1 <- ggplot(data = ais_timerange_scenes, aes(x = time_diff_min_signed)) +
  # histograma con densidad
  geom_histogram(binwidth = 1, fill = "skyblue3", color = "white") +
  # curva de densidad
  # geom_density(colour = "#FF8828", fill = "#F8D6BA", alpha = 0.75, size = 0.75) +
  # línea de la media
  geom_vline(aes(xintercept = mean(time_diff_min_signed, na.rm = TRUE)),
             color = "grey20", linetype = "dashed", size = 0.4) +
  # 0 line
  # geom_vline(aes(xintercept = 0),
  #            color = "grey70", size = 0.5, alpha = 0.7) +
  # escala X cada 5 minutos
  scale_x_continuous(breaks = seq(
    floor(min(ais_timerange_scenes$time_diff_min_signed, na.rm = TRUE) / 5) * 5,
    ceiling(max(ais_timerange_scenes$time_diff_min_signed, na.rm = TRUE) / 5) * 5,
    by = 5
  )) +
  
  annotate("text",
           x = min(ais_timerange_scenes$time_diff_min_signed, na.rm = TRUE),
           y = Inf,
           label = "a)",
           hjust = 0, vjust = 2.5,
           size = 5) +
  
  # etiquetas
  ylab("N.º AIS raw last broadcasted positions") + xlab("Time difference (min)") +
  # tema
  theme_bw() +
  theme(
    axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial"),
    axis.ticks = element_line(size = 0.4),
    axis.ticks.length = unit(-5, "pt"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank()
  )

p1




# export figure as


# Analaysis at scenes levels

# --- 1. Calcular métricas por escena ---
mean_time_diff_scene <- ais_timerange_scenes %>%
  group_by(img_ID) %>%
  summarise(
    mean_time_diff_signed = mean(time_diff_min_signed, na.rm = TRUE),
    mean_time_diff_abs = mean(abs(time_diff_min_signed), na.rm = TRUE),
    max_time_diff_abs = max(abs(time_diff_min_signed), na.rm = TRUE),
    n_vessels = n()
  )



summary(mean_time_diff_scene$mean_time_diff_signed)
summary(abs(mean_time_diff_scene$mean_time_diff_signed))


# --- 2. Histograma de la media signed por escena ---



p2 <- ggplot(mean_time_diff_scene, aes(x = mean_time_diff_signed)) +
  geom_histogram(binwidth = 0.5, fill = "tan1", color = "white") +
  
  geom_vline(aes(xintercept = mean(mean_time_diff_signed, na.rm = TRUE)),
             color = "grey20", linetype = "dashed", size = 0.4) +
  
  scale_x_continuous(
    limits = c(-3.5, 3.5),
    breaks = seq(-4, 4, by = 1)
  ) +
  
  ylab("N.º Scenes") + 
  xlab("Mean time difference (min)") +
  # b)
  annotate("text",
           x = -3.3,  # borde izquierdo visible
           y = Inf,
           label = "b)",
           hjust = -0.5, vjust = 2.5,
           size = 5) +
  
  theme_bw() +
  theme(
    axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial"),
    axis.ticks = element_line(size = 0.4),
    axis.ticks.length = unit(-5, "pt"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank()
  )

p2



# combine plots p1 + p2
p <- grid.arrange(p1, p2, nrow = 1)
p


# export / save plot
p_png <- "data/output/plots/sup_fig_ais_timerange.png"
p_svg <- "data/output/plots/sup_fig_ais_timerange.png"
ggsave(p_png, p, width= 20, height=10, units="cm", dpi=350, bg = "white")
ggsave(p_svg, p, width= 20, height=10, units="cm", dpi=350, bg = "white")





# # --- 3. Histograma de la media absoluta por escena ---
# ggplot(mean_time_diff_scene, aes(x = mean_time_diff_abs)) +
#   geom_histogram(binwidth = 1, fill = "purple", color = "white") +
#   labs(
#     title = "Mean Absolute Time Difference per Scene",
#     x = "Mean absolute time difference (minutes)",
#     y = "Number of scenes"
#   )

# --- 4. Escenas con mayor desfase signed promedio ---
mean_time_diff_scene %>%
  arrange(desc(abs(mean_time_diff_signed))) %>%
  head(10)

















