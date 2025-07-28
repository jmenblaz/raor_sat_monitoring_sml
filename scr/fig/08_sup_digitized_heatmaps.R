

#-------------------------------------------------------------------------------
# 04 - Temporal plots 
# ------------------------------------------------------------------------------

# Digitized coverage heatmaps panel

# 1) Number of scenes per day: heatmap
# 2) % of coverage area of total study area
# 3) % of coverage area of fishing area


library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(paletteer)
library(colorspace)
library(gridExtra)

# Load data for prepare heatamps
# using daily effort dataframe for daily data
sat_f <- read.csv("data/output/fishing_effort/satellite_fishing_effort_data.csv")







# start_date <- as.Date("2023-08-25")
# end_date <- as.Date("2023-09-20")
# days <- seq(from = start_date, to = end_date, by = "day")
# days <- format(days, "%m-%d")


# Forma timestamp
sat_f$timestamp <- as.Date(sat_f$timestamp)


# -----------------------------------------------------------------------------
# Heatmap 1: Nnumber of scenes per day  ----------------------------------------


# prepare data for plot
sat_f_heatmap <- sat_f %>%
  mutate(
    year = year(timestamp),
    month_day = format(timestamp, "%m-%d")
  ) %>%
  filter(month_day >= "08-25" & month_day <= "09-20") %>%
  mutate(
    month_day = factor(month_day, levels = rev(sort(unique(month_day)))),
    fontface_label = ifelse(number_scenes > 2, "bold", "plain") # values higher than 2 bold
  )

# two decimals
sat_f_heatmap <- sat_f_heatmap %>%
  mutate(
    digitized_fa_cover = round(digitized_fa_cover, 1),
    digitized_sa_cover = round(digitized_sa_cover, 1)
  )


# color ramp
colRamp_scenes <- colorRampPalette(c('#FFFFFF', '#FDB863', '#E66101','tan4'))(50)

# Plot
p1 <- ggplot(sat_f_heatmap, aes(y = month_day, x = factor(year), fill = number_scenes)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = number_scenes, fontface = fontface_label), color = "#FFFFFF", size = 3.25, family = "Arial") +
  scale_fill_gradientn(colors = colRamp_scenes, na.value = "#FFFFFF") +
  scale_y_discrete(position = "right") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial"),
    axis.text.y.right = element_text(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

#p1




# -----------------------------------------------------------------------------
# Heatmap 2: % of study area digitized per day   -----------------------------

library(dplyr)
library(ggplot2)
library(paletteer)
library(colorspace)


pal_original <- as.character(paletteer_c("ggthemes::Red-Green-Gold Diverging", 30))

# Quitar el canal alfa (últimos dos caracteres)
pal_rgb <- substr(pal_original, 1, 7)

# Aplicar lighten
colRamp_scenes <- lighten(pal_rgb, amount = 0.15)
colRamp_scenes <- colRamp_scenes[4:(length(colRamp_scenes)-5)]

# Crear nueva variable sin ceros
sat_f_heatmap <- sat_f_heatmap %>%
  mutate(
    digitized_sa_cover_no0 = ifelse(digitized_sa_cover == 0, NA, digitized_sa_cover)
  )

# Plot
p2 <- ggplot(sat_f_heatmap, aes(y = month_day, x = factor(year), fill = digitized_sa_cover_no0)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = digitized_sa_cover), color = "#FFFFFF", size = 3, family = "Arial", fontface = "bold") +
  scale_fill_gradientn(colors = colRamp_scenes, na.value = "#FFFFFF") +
  scale_y_discrete(position = "right") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial"),
    axis.text.y.right = element_text(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

p2




# -----------------------------------------------------------------------------
# Heatmap 3: % of fishing area digitized per day   -----------------------------


# Crear nueva variable sin ceros
sat_f_heatmap <- sat_f_heatmap %>%
  mutate(
    digitized_fa_cover_no0 = ifelse(digitized_fa_cover == 0, NA, digitized_fa_cover)
  )

# Plot
p3 <- ggplot(sat_f_heatmap, aes(y = month_day, x = factor(year), fill = digitized_fa_cover_no0)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = digitized_fa_cover), color = "#FFFFFF", size = 3, family = "Arial", fontface = "bold") +
  scale_fill_gradientn(colors = colRamp_scenes, na.value = "#FFFFFF") +
  scale_y_discrete(position = "right") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial"),
    axis.text.y.right = element_text(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

p3



# combine p1, p2, p3 
p <- grid.arrange(p1, p2, p3, ncol = 3)
p

# save plot
p_png <- "fig/sup_fig_heatmaps.png"
p_svg <- "fig/sup_fig_heatmaps.svg"
ggsave(p_png, p, width=26, height=23, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=26, height=23, units="cm", dpi=350, bg="white")
