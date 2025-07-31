
# Descript analysis and results

library(dplyr)

# 1) check potential fishing boats ------------------------
sat <- read.csv("data/output/fishing_effort/cloudpoint/satellite_fishing_effort.csv")

# remove potencial duplicates
sat <- sat %>% distinct()

# filter by navigation status, duplicated vessel in satellite pass and lowCertainity
sat <- sat %>% filter(sat$duplicated == "FALSE") # 1453
sat <- sat %>% filter(sat$lowCertainty == "FALSE") # 1433
sat <- sat %>% filter(sat$navigationStatus == "anchor") 

nrow(sat) # number of potential fishing boat

# Ensure 'day' is Date
sat$day <- as.Date(sat$day)

# Extract month-day
Sys.setlocale("LC_TIME", "C")  # or "en_US.UTF-8" if needed
sat$month_day <- format(sat$day, "%m-%d")
# Extract weekday
sat$weekday <- weekdays(sat$day)
# Allowed fishing days after season opens
allowed_days <- c("Tuesday", "Friday", "Saturday", "Sunday")
# Filter: first 6 days of September AND allowed fishing days
sat_filtered <- subset(sat,
                       month_day %in% sprintf("09-%02d", 1:6) &
                         sat$weekday %in% allowed_days)
# Count vessels per full date
vessels_per_day <- table(sat_filtered$day)
# Compute mean
daily_mean <- mean(vessels_per_day)
# Show result
daily_mean


















summary(sat$length_m)
sd(sat$length_m)
hist(sat$length_m)

# New supplementary material of the potential fishing boat lengths digitalized
# p1 


library(ggplot2)
library(grid)  # para unit()
library(gridExtra)

# Crear histograma de 'length_m'
p1 <- ggplot(sat, aes(x = length_m)) +
  geom_histogram(binwidth = 1.4, fill = "tan1", color = "white", size = 0.3) + 
  labs(
    x = "Vessel length (m)",
    y = "N.° Vessels"
  ) +
  geom_vline(aes(xintercept = mean(length_m, na.rm = TRUE)),
             color = "grey20", linetype = "dashed", size = 0.4) +
  
  annotate("text", x = 1.5, y = 250, label = "b)", vjust = 1.5,
          hjust = 0, fontface = "bold", size = 4, family = "Arial") +
  
  theme_bw() +
  theme(
    axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial", angle = 0, hjust = 1, vjust = 0.5),
    axis.ticks = element_line(size = 0.4),
    axis.ticks.length = unit(-5, "pt"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(11, "pt"),
    legend.key.height = unit(5, "pt"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5)
  )

p1

# # export / save plot
# p_png <- "fig/histogram_fishing_vessel_dig.png"
# p_svg <- "fig/histogram_fishing_vessel_dig.png"
# ggsave(p_png, p1, width= 11, height=11, units="cm", dpi=350, bg = "white")
# ggsave(p_svg, p1, width= 11, height=11, units="cm", dpi=350, bg = "white")



# plot potential fishing AIS vessel lengths ------------------

ais <- read.csv("data/output/fishing_effort/cloudpoint/ais_fishing_effort.csv")
ais <- ais %>% filter(ais$type_name != "Passenger")

ais <- ais %>% filter(ais$length < 45)
ais <- ais %>% filter(ais$speed < 8)

ais <- ais %>% filter(ais$length > 0)

nrow(ais)


# Crear histograma de 'length_m'
p2 <- ggplot(ais, aes(x = length)) +
  geom_histogram(binwidth = 1.4, fill = "skyblue3", color = "white", size = 0.3) + 
  labs(
    x = "Vessel length (m)",
    y = "N.° Vessels"
  ) +
  geom_vline(aes(xintercept = mean(length, na.rm = TRUE)),
             color = "grey20", linetype = "dashed", size = 0.4) +
  
  annotate("text", x = 1.5, y = 15, label = "a)", vjust = 1.5,
           hjust = 0, fontface = "bold", size = 4, family = "Arial") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
    axis.text.y = element_text(size = 9, family = "Arial"),
    axis.text.x = element_text(size = 9, family = "Arial", angle = 0, hjust = 1, vjust = 0.5),
    axis.ticks = element_line(size = 0.4),
    axis.ticks.length = unit(-5, "pt"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(11, "pt"),
    legend.key.height = unit(5, "pt"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5)
  )

p2


# # export / save plot
# p_png <- "fig/histogram_fishing_vessel_dig.png"
# p_svg <- "fig/histogram_fishing_vessel_dig.png"
# ggsave(p_png, p2, width= 11, height=11, units="cm", dpi=350, bg = "white")
# ggsave(p_svg, p2, width= 11, height=11, units="cm", dpi=350, bg = "white")


# combine
p <- grid.arrange(p2, p1, nrow = 1)
p


# export / save plot
p_png <- "fig/histogram_fishing_vessel_dig.png"
p_svg <- "fig/histogram_fishing_vessel_dig.svg"
ggsave(p_png, p, width= 20, height=11, units="cm", dpi=350, bg = "white")
ggsave(p_svg, p, width= 20, height=11, units="cm", dpi=350, bg = "white")




