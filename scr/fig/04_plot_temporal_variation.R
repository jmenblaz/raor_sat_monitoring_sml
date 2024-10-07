#-------------------------------------------------------------------------------
# 04 - Temporal plots 
# ------------------------------------------------------------------------------

# 1) Number of vessel per day and year in satellite images
# 2a) Mean number of vessel per day by method (AIS, Satellite, Fixed Camera)
# 2b) Fishing effor/km2 and day - number of vessel per Km2 filtering ships 200m and nontake area

# 3) Minutes from sunrise of timestamps Planet scene
# 4) Mean Number of ships identify by methods in week days in the study time period
# 5) Daily annual temporal variation of ships with AIS and Satellite detection 
# 6) Cloud coverage heatmap

# 7) Number of fishing ships identify by methods in week days --------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

# 1) Number of vessel per day and year in satellite images

# Prepare data:

# raw digitalized
df <- read.csv("data/output/shipProc.csv", sep = ";")

# filter
df <- df %>% 
  filter(duplicated == FALSE) 
  # %>%
  # filter(navigationStatus == "anchor")

# year
df$acquired <- as.POSIXct(df$acquired)
df$year <- year(df$acquired)

# Contar el número de registros por día sin tener en cuenta el año
registros_por_dia <- df %>%
  mutate(dia = format(as.Date(acquired), "%m-%d")) %>%
  group_by(dia, year) %>%
  summarise(num_registros = n())
# daily mean
media_por_dia <- registros_por_dia %>%
  group_by(dia) %>%
  summarise(median_registros = mean(num_registros))


# plot
p1 <- ggplot(data = registros_por_dia, aes(x = dia, y = num_registros, group = year, color = factor(year))) +
  geom_line(size = 0.75, alpha = 0.2) +
  geom_point(size = 2, alpha = 0.4) +
  # mean line
  # stat_summary(aes(y = num_registros, group = 1), fun.y = median, colour = "grey15", geom = "smooth", size = 1.2)  +
  
  #geom smooth
  geom_smooth(data = registros_por_dia, aes(x = dia, y = num_registros, group = FALSE), 
              num_remethod = "loess", formula = 'y ~ x', span = 0.17, se = F, size = 0.8, colour = "grey20",
              inherit.aes = TRUE) +
  
  # add fishing access
  geom_vline(xintercept = 8, linetype="dashed", color="grey40", size = 0.35, alpha = 0.45) +
  
  # labels
  labs(x = "", y = "N.º Ships", color = "Year") +
  # theme
  theme_bw() +
  theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
        axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
        # axis labels
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 6.5, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(-5, "pt"),  # negative lenght -> ticks inside the plot 
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        # legend
        legend.position = c(.70, .90), 
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(11, "pt"),
        legend.key.height = unit(5, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6.5)
        )

p1

# save plot
p_png <- "fig/fig4_1.png"
p_svg <- "fig/fig4_1.svg"
ggsave(p_png, p1, width=14, height=8, units="cm", dpi=350, bg="white")
ggsave(p_svg, p1, width=14, height=8, units="cm", dpi=350, bg="white")














# 2) Mean number of vessel per day by method (AIS, Satellite)
# Create days (use one year as a model)
start_date <- as.Date("2023-08-25")
end_date <- as.Date("2023-09-20")
days <- seq(from = start_date, to = end_date, by = "day")
days <- format(days, "%m-%d")



# 2.1 ) Prepare data--------------------------------------
# 2.1.1) Satellite ----------
df <- read.csv("data/output/shipProc.csv", sep = ";")
# filter
df <- df %>% 
  filter(duplicated == FALSE) %>%
  filter(navigationStatus == "anchor")

# year
df$acquired <- as.Date(df$acquired, format="%d/%m/%Y") # as date
df$acquired <- format(df$acquired, "%Y/%m/%d") # format date
df$year <- year(df$acquired)
# Contar el número de registros por día sin tener en cuenta el año
df <- df %>%
  mutate(dia = format(as.Date(acquired), "%m-%d")) %>%
  group_by(dia, year) %>%
  summarise(num_registros = n())

# match 
df <- df %>%
  group_by(dia) %>%
  mutate(numeracion_secuencial = match(dia, days))

# as numeric
df$num_registros<- as.numeric(df$num_registros)
df$numeracion_secuencial <- as.numeric(df$numeracion_secuencial)



# 2.1.2) AIS data ---------------
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# filter potential AIS position in the same timestamp
ais <- ais %>%
  distinct(mmsi, timestamp, .keep_all = TRUE)

# year
ais$timestamp <- as.POSIXct(ais$timestamp)
# ais$year <- year(ais$timestamp)
# Contar el número de registros por día sin tener en cuenta el año
ais <- ais %>%
  mutate(day = format(as.Date(timestamp), "%m-%d")) %>%
  group_by(day) %>%
  summarise(num_registros = n())

# match 
ais <- ais %>%
  group_by(day) %>%
  mutate(numeracion_secuencial = match(day, days))

# as numeric
ais$num_registros<- as.numeric(ais$num_registros)
ais$numeracion_secuencial <- as.numeric(ais$numeracion_secuencial)


# -----------------------------------------------------------------------------
# 2.2A Plot ------------------------------------------- Number of ships per day

p2a <- ggplot() +
  # Satellite
  geom_point(data = df, aes(x = numeracion_secuencial, y = num_registros), color = "#FF8828", size = 2, alpha = 0.35) +
  geom_smooth(data = df, aes(x = numeracion_secuencial, y = num_registros), size = 1.15, span = 0.2, 
              color = "darkorange4", se = F, alpha = 0.1) +
  geom_smooth(data = df, aes(x = numeracion_secuencial, y = num_registros), size = 0.75, span = 0.2, 
              color = "#FF8828", fill = "#F8D6BA") +
  # AIS
  geom_point(data = ais, aes(x = numeracion_secuencial, y = num_registros), color = "deepskyblue3", size = 2, alpha = 0.35) +
  geom_smooth(data = ais, aes(x = numeracion_secuencial, y = num_registros), size = 1.15, span = 0.2, 
              color = "deepskyblue4", se = F, alpha = 0.1) +
  geom_smooth(data = ais, aes(x = numeracion_secuencial, y = num_registros), size = 0.75, span = 0.2, 
              color = "deepskyblue3", fill = "lightblue3") +

  # add line for fishing time-restriction
  geom_vline(xintercept = 8, linetype="dashed", color="grey40", size = 0.35, alpha = 0.45) +
  # labels sequential days created
  scale_x_continuous(breaks = 1:length(days), labels = days) +
  # label text
  ylab("N.º Ships") + xlab("") +
  # theme
  theme_bw() +
  theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
        axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
        # axis labels
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 6.5, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(-5, "pt"),  # negative lenght -> ticks inside the plot 
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        # legend
        legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(11, "pt"),
        legend.key.height = unit(5, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6.5)
  )

p2a

# save plot
p_png <- "fig/fig4_2a.png"
p_svg <- "fig/fig4_2a.svg"
ggsave(p_png, p2a, width=14, height=8, units="cm", dpi=350, bg="white")
ggsave(p_svg, p2a, width=14, height=8, units="cm", dpi=350, bg="white")





#------------------------------------------------------------------------------
# 2.2.B Plot ------------------------------------------- Fishing effort per day

# 2) Mean number of vessel per day by method (AIS, Satellite)
# Create days (use one year as a model)
start_date <- as.Date("2023-08-25")
end_date <- as.Date("2023-09-20")
days <- seq(from = start_date, to = end_date, by = "day")
days <- format(days, "%m-%d")


# 2.1 ) Prepare data--------------------------------------
# 2.1.1) Satellite ----------
method <- "satellite"
path <- "data/output/fishing_effort/"
df <- read.csv(paste0(path, method,"_fishing_effort_data.csv"))

df$ship_denskm2_fa <- df$ships/6.980401

# year
df$timestamp <- as.POSIXct(df$timestamp)
df$year <- year(df$timestamp)

# fishing effort by day
df <- df %>%
  group_by(day, year) %>%
  summarise(ship_denskm2_fa = mean(ship_denskm2_fa))

# match 
df <- df %>%
  group_by(day) %>%
  mutate(numeracion_secuencial = match(day, days))

# as numeric
df$ship_denskm2_fa <- as.numeric(df$ship_denskm2_fa)
df$numeracion_secuencial <- as.numeric(df$numeracion_secuencial)



# 2.B) AIS data -------------------
method <- "ais"
ais <- read.csv(paste0(path, method,"_fishing_effort_data.csv"))

ais$ship_denskm2_fa <- ais$ships/6.980401

# year
ais$timestamp <- as.POSIXct(ais$timestamp)
ais$year <- year(ais$timestamp)
# fishing effort by day
ais <- ais %>%
  group_by(day, year) %>%
  summarise(ship_denskm2_fa = mean(ship_denskm2_fa))

# match 
ais <- ais %>%
  group_by(day) %>%
  mutate(numeracion_secuencial = match(day, days))

# as numeric
ais$ship_denskm2_fa <- as.numeric(ais$ship_denskm2_fa)
ais$numeracion_secuencial <- as.numeric(ais$numeracion_secuencial)


p2b <- ggplot() +
  # Satellite
  geom_point(data = df, aes(x = numeracion_secuencial, y = ship_denskm2_fa), color = "#FF8828", size = 2, alpha = 0.35) +
  geom_smooth(data = df, aes(x = numeracion_secuencial, y = ship_denskm2_fa), size = 1.15, span = 0.15, 
              color = "darkorange4", se = F, alpha = 0.1) +
  geom_smooth(data = df, aes(x = numeracion_secuencial, y = ship_denskm2_fa), size = 0.75, span = 0.15, 
              color = "#FF8828", fill = "#F8D6BA") +
  # AIS
  geom_point(data = ais, aes(x = numeracion_secuencial, y = ship_denskm2_fa), color = "deepskyblue3", size = 2, alpha = 0.35) +
  geom_smooth(data = ais, aes(x = numeracion_secuencial, y = ship_denskm2_fa), size = 1.15, span = 0.05, 
              color = "deepskyblue4", se = F, alpha = 0.1) +
  geom_smooth(data = ais, aes(x = numeracion_secuencial, y = ship_denskm2_fa), size = 0.75, span = 0.05, 
              color = "deepskyblue3", fill = "lightblue3") +

  # add line for fishing time-restriction
  geom_vline(xintercept = 8, linetype="dashed", color="grey40", size = 0.35, alpha = 0.45) +
  # labels sequential days created
  scale_x_continuous(breaks = 1:length(days), labels = days) +
  # y axis
  # ylim(-2, 30) +
  coord_cartesian(ylim = c(-2, 12)) +
  # label text
  # y
  ylab((expression("Fishing effort (N.º ships/km"^2*" of fishing area)"))) + 
  # x
  xlab("") +
  # theme
  theme_bw() +
  theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
        axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
        # axis labels
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 6.5, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(-5, "pt"),  # negative lenght -> ticks inside the plot 
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        # legend
        legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(11, "pt"),
        legend.key.height = unit(5, "pt"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6.5)
  )

p2b

# save plot
p_png <- "fig/fig4_2b.png"
p_svg <- "fig/fig4_2b.svg"
ggsave(p_png, p2b, width=14, height=8, units="cm", dpi=350, bg="white")
ggsave(p_svg, p2b, width=14, height=8, units="cm", dpi=350, bg="white")






# 3) Minutes from sunrise of timestamps Planet scene ---------------------------

# data
# Note: min_from_sunrise processed in other script (04_sunrise_calculator)
df <- read.csv("data/output/scene_data.csv")

p3 <- ggplot(data = df, aes(x = min_from_sunrise)) +
  # add histogram
  geom_histogram(aes(y=..density..), fill="grey80") +
  # add density plot
  geom_density(colour = "#FF8828", fill="#F8D6BA", alpha = 0.75, size = 0.75) +
  # mean line
  geom_vline(aes(xintercept=mean(min_from_sunrise)),
             color="grey20", linetype="dashed", size = 0.4) +
  # labels text
  ylab("density") + xlab("minutes") +
  # theme
  theme_bw() +
  theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
        axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
        # axis labels
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 9, family = "Arial"),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(-5, "pt"),  # negative lenght -> ticks inside the plot 
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank())
p3

# save plot
p_png <- "fig/fig4_3.png"
p_svg <- "fig/fig4_3.svg"
ggsave(p_png, p3, width=9, height=8, units="cm", dpi=300, bg="white")
ggsave(p_svg, p3, width=9, height=8, units="cm", dpi=300, bg="white")





# 4) Mean Number of ships identify by methods in week days --------------------
#    in the study time period

# 4.1 ) Prepare data--------------------------------------
# 4.1.1) Satellite ----------
df <- read.csv("data/output/shipProc.csv", sep = ";")
# filter
df <- df %>% 
  filter(duplicated == FALSE) 
# filter(navigationStatus == "anchor")
str(df)
# year
df$acquired <- as.POSIXct(df$acquired, format = "%d/%m/%Y %H:%M")
df$year <- year(df$acquired)
# day week (english)
Sys.setlocale("LC_TIME", "English")
df$wday <- lubridate::wday(df$acquired, label = TRUE, abbr = FALSE)

# records by weekday
df <- df %>%
  group_by(year, wday) %>%
  summarise(num_registros = n())
# df <- na.omit(df)

# numeric sequentation of weekdays
df <- df %>%
  mutate(wday_num = case_when(
    wday == "Monday" ~ 1,
    wday == "Tuesday" ~ 2,
    wday == "Wednesday" ~ 3,
    wday == "Thursday" ~ 4,
    wday == "Friday" ~ 5,
    wday == "Saturday" ~ 6,
    wday == "Sunday" ~ 7
  ))

# as numeric
df$wday_num <- as.numeric(df$wday_num)
df$num_registros <- as.numeric(df$num_registros)


# 4.1.2) AIS data ---------------
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# filter potential AIS position in the same timestamp
ais <- ais %>%
  distinct(mmsi, timestamp, .keep_all = TRUE)

# year
ais$timestamp <- as.POSIXct(ais$timestamp)
ais$year <- year(ais$timestamp)

# day week (english)
Sys.setlocale("LC_TIME", "English")
ais$wday <- lubridate::wday(ais$timestamp, label = TRUE, abbr = FALSE)

# records by weekday
ais <- ais %>%
  group_by(year, wday) %>%
  summarise(num_registros = n())

ais <- na.omit(ais)

# numeric sequentation of weekdays
ais <- ais %>%
  mutate(wday_num = case_when(
    wday == "Monday" ~ 1,
    wday == "Tuesday" ~ 2,
    wday == "Wednesday" ~ 3,
    wday == "Thursday" ~ 4,
    wday == "Friday" ~ 5,
    wday == "Saturday" ~ 6,
    wday == "Sunday" ~ 7
  ))

# as numeric
ais$wday_num <- as.numeric(ais$wday_num)
ais$num_registros <- as.numeric(ais$num_registros)


days <-  c("Monday", "Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday")

# 4.2 Plot
p4 <- ggplot() +
  # Satellite
  geom_point(data = df, aes(x = wday_num, y = num_registros), color = "#FF8828", size = 2, alpha = 0.35) +
  geom_smooth(data = df, aes(x = wday_num, y = num_registros), size = 1.15, span = 0.2, 
              color = "darkorange4", se = F, alpha = 0.1) +
  geom_smooth(data = df, aes(x = wday_num, y = num_registros), size = 0.75, span = 0.2, 
              color = "#FF8828", fill = "#F8D6BA") +
  # AIS
  geom_point(data = ais, aes(x = wday_num, y = num_registros), color = "deepskyblue3", size = 2, alpha = 0.35) +
  geom_smooth(data = ais, aes(x = wday_num, y = num_registros), size = 1.15, 
              color = "deepskyblue4", se = F, alpha = 0.1) +
  geom_smooth(data = ais, aes(x = wday_num, y = num_registros), size = 0.75, 
              color = "deepskyblue3", fill = "lightblue3") +
  
  # labels sequential days created
  scale_x_continuous(breaks = 1:length(days), labels = days) +
  # label text
  ylab("N.º Ships") + xlab("") +
  # theme
  theme_bw() +
  theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
        axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
        # axis labels
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 8, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(-5, "pt"),  # negative lenght -> ticks inside the plot 
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
  )

p4


# save plot
p_png <- "fig/fig4_4.png"
p_svg <- "fig/fig4_4.svg"
ggsave(p_png, p4, width=9, height=10, units="cm", dpi=350, bg="white")
ggsave(p_svg, p4, width=9, height=10, units="cm", dpi=350, bg="white")











# ----------------------------------------------------------------------------
# 5) Daily annual temporal variation of ships with AIS and Satellite detection 

# Create list of days----------------------
years <- c("2016","2017","2018","2019","2020","2021","2022","2023")

days <- data.frame()
for (y in years) {
  start_date <- as.Date(paste0(y,"-08-25"))
  end_date <- as.Date(paste0(y,"-09-20"))
  ds <- seq(from = start_date, to = end_date, by = "day")
  ds <- data.frame(ds)
  days <- rbind(days, ds)
}
rm(ds)

days <-  c("Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday")



# 5.1 ) Prepare data  for ploting --------------------------------------------
# 5.1.1) Satellite ----------

# Create days (use one year as a model)
start_date <- as.Date("2023-08-25")
end_date <- as.Date("2023-09-20")
days <- seq(from = start_date, to = end_date, by = "day")
days <- format(days, "%m-%d")


# 5.1 ) Prepare data--------------------------------------
# 5.1.1) Satellite ----------
df <- read.csv("data/output/shipProc.csv", sep = ";")
# filter
df <- df %>% 
  filter(duplicated == FALSE)

# date format
df$acquired <- as.POSIXct(df$acquired, format="%d/%m/%Y") # as date

# Contar el número de registros por día sin tener en cuenta el año
df <- df %>%
  group_by(acquired) %>%
  summarise(num_registros = n())

# extract year day info
df$year <- year(df$acquired)
df$day <- format(df$acquired, "%m-%d")

# match day - add sequential number day
df <- df %>%
  mutate(numeracion_secuencial = match(day, days))

# as numeric
df$num_registros<- as.numeric(df$num_registros)
df$numeracion_secuencial <- as.numeric(df$numeracion_secuencial)

# day week (english)
Sys.setlocale("LC_TIME", "English")
df$wday <- lubridate::wday(df$acquired, label = TRUE, abbr = FALSE)

# numeric sequentation of weekdays
df <- df %>%
  mutate(wday_num = case_when(
    wday == "Monday" ~ 1,
    wday == "Tuesday" ~ 2,
    wday == "Wednesday" ~ 3,
    wday == "Thursday" ~ 4,
    wday == "Friday" ~ 5,
    wday == "Saturday" ~ 6,
    wday == "Sunday" ~ 7
  ))

df$year <- as.factor(df$year)


#-------------------------------------------------------------------------------
# 5.1.2) AIS data preapre for plotting ---------------

ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# filter potential AIS position in the same timestamp
ais <- ais %>%
  distinct(mmsi, timestamp, .keep_all = TRUE)

ais$timestamp <- as.POSIXct(ais$timestamp, format="%Y-%m-%d") # as date

# Contar el número de registros por día sin tener en cuenta el año
ais <- ais %>%
  group_by(timestamp) %>%
  summarise(num_registros = n())

# extract year day info
ais$year <- year(ais$timestamp)
ais$day <- format(ais$timestamp, "%m-%d")

# match day - add sequential number day
ais <- ais %>%
  mutate(numeracion_secuencial = match(day, days))

# as numeric
ais$num_registros<- as.numeric(ais$num_registros)
ais$numeracion_secuencial <- as.numeric(ais$numeracion_secuencial)

# day week (english)
Sys.setlocale("LC_TIME", "English")
ais$wday <- lubridate::wday(ais$timestamp, label = TRUE, abbr = FALSE)

# numeric sequentation of weekdays
ais <- ais %>%
  mutate(wday_num = case_when(
    wday == "Monday" ~ 1,
    wday == "Tuesday" ~ 2,
    wday == "Wednesday" ~ 3,
    wday == "Thursday" ~ 4,
    wday == "Friday" ~ 5,
    wday == "Saturday" ~ 6,
    wday == "Sunday" ~ 7
  ))

ais$year <- as.factor(ais$year)



# 5.2.1 Plot Satellite data

p51 <- ggplot() +
  # Satellite
 geom_point(data = df, aes(x = numeracion_secuencial, y = num_registros), color = "#FF8828", 
            size = 1.25, alpha = 0.35) +
 geom_smooth(data = df, aes(x = numeracion_secuencial, y = num_registros), size = 1., span = 0.6, 
              color = "darkorange4", se = F, alpha = 0.1) +
  geom_smooth(data = df, aes(x = numeracion_secuencial, y = num_registros), size = 0.7, span = 0.6, 
              color = "#FF8828", fill = "#F8D6BA") +
  #coord limit
   ylim(c(-10, 150)) +
   coord_cartesian(ylim = c(-10, 150)) +
  # line fishing van 01/09
  # geom_vline(xintercept = 8, linetype="dashed", color="grey40", size = 0.35, alpha = 0.45) +
  # face grid

  facet_wrap(~df$year, ncol = 8) +
  
  # labels sequential days created
  # scale_x_continuous(breaks = 1:length(days), labels = days) +
  # label text
  ylab("N.º Ships") + xlab("") +
  # face wrap strip theme
  theme() +
  # theme
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis labels
        axis.text.y = element_blank(),
        # axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 8, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),

        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(5, "pt"),  # negative lenght -> ticks inside the plot
        axis.ticks.length.x = unit(0, "pt"),
        # strips
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, family = "Arial", colour = "black"),
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
  )

p51


# 5.2.2 Plot AIS
p52 <- ggplot() +
  # AIS
  geom_point(data = ais, aes(x = numeracion_secuencial, y = num_registros), color = "deepskyblue3", 
             size = 1.25, alpha = 0.35) +
  geom_smooth(data = ais, aes(x = numeracion_secuencial, y = num_registros), size = 1, span = 0.6, 
              color = "deepskyblue4", se = F, alpha = 0.1) +
  geom_smooth(data = ais, aes(x = numeracion_secuencial, y = num_registros), size = 0.7, span = 0.6, 
              color = "deepskyblue3", fill = "lightblue3") +
  
  # ylim(c(-3, 20)) +
  coord_cartesian(ylim = c(-2, 25)) +
  
  # face grid
  facet_wrap(~ais$year, ncol = 8) +
  
  # labels sequential days created
  scale_x_continuous(n.breaks = 7) +
  # scale_x_discrete(breaks = c(1,5,8,12,17,22,27)) +
  # label text
  ylab("N.º Ships") + xlab("") +
  # theme
  theme_bw() +
  # face wrap strip theme
  theme(strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "white")) +
  # general theme
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis labels
        axis.text.y = element_blank(),
        # axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 8, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(5, "pt"), # negative lenght -> ticks inside the plot
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
  )

p52

# combine plots
p <- grid.arrange(p51, p52, ncol = 1)
p



# save plot
p_png <- "fig/fig4_5.png"
p_svg <- "fig/fig4_5.svg"
ggsave(p_png, p, width=18, height=9, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=18, height=9, units="cm", dpi=350, bg="white")






# -----------------------------------------------------------------------------
# 6) Cloudcoverage heatmap -----------------------------------------------------

start_date <- as.Date("2023-08-25")
end_date <- as.Date("2023-09-20")
days <- seq(from = start_date, to = end_date, by = "day")
days <- format(days, "%m-%d")


# 6.1 ) Prepare data--------------------------------------
df <- read.csv("data/output/scene_data.csv")

# create dates sequence
fechas <- seq(from = as.Date("2016-08-25"), to = as.Date("2023-09-20"), by = "day")

# summary by day
df_resumen <- df %>%
  group_by(date) %>%
  summarise(
    total_clouds_area = sum(clouds_area_km2, na.rm = TRUE),
    total_digitized_area = sum(scene_area_km2, na.rm = TRUE)
  )

# calculate % of clouds
df_resumen <- df_resumen %>%
  mutate(
    perc_clouds = (total_clouds_area / total_digitized_area) * 100,
    date = as.Date(date)  # Asegúrate de que la fecha esté en formato Date
  )

# New data frame for join
df_final <- data.frame(date = fechas) %>%
  left_join(df_resumen, by = "date")

# filter only study time
df_final <- df_final %>%
  filter(month(date) == 8 & day(date) >= 25 | month(date) == 9 & day(date) <= 20)

# format data
df_final <- df_final %>%
  mutate(
    year = year(date),                   # Extrae el año
    month_day = format(date, "%m-%d"), # Extrae mes y día en formato MM-DD
    perc_clouds = round(perc_clouds, 2) # round decimals
  )
# reorder values of days for plotting
df_final <- df_final %>%
  mutate(month_day = factor(month_day, levels = rev(unique(month_day)))) 


# plot - heatmap ------------------ 

colRamp <- colorRampPalette(c('#B9D3EE','#FFFAFA','#C7C7C7','#ABABAB','#8B8682','#404040'))(50)







p6 <- ggplot(df_final, aes(y = month_day, x = factor(year), fill = perc_clouds)) +
        # tiles color and border,
        geom_tile(color = "white",
                  lwd = 1) +
        # tiles labels
        geom_text(aes(label = perc_clouds), color = "grey20", size = 3, family = "Arial") +
        # fill color
        scale_fill_gradientn(colors = colRamp,
                             # guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title = ""),
                             na.value = "#FFFFFF") +
        scale_y_discrete(position = "right") +  # Y axis on right
        # theme 
        theme_bw() +
        # general theme
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # axis labels
              axis.text.y = element_text(size = 9, family = "Arial"),
              axis.text.x = element_text(size = 10, family = "Arial"),
              axis.text.y.right = element_text(),     # Etiquetas del eje Y en el lado derecho
              axis.ticks = element_blank(),
              # panel
              panel.border = element_rect(color = "black", fill = NA, size = 1.1),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              # legend
              legend.position =  "none")


p6



# save plot
p_png <- "fig/fig2_3.png"
p_svg <- "fig/fig2_3.svg"
ggsave(p_png, p6, width=10, height=22, units="cm", dpi=350, bg="white")
ggsave(p_svg, p6, width=10, height=22, units="cm", dpi=350, bg="white")
























# 7) Number of fishing ships identify by methods in week days --------------------
#    in the study time period

# 7.1 ) Prepare data--------------------------------------
# 7.1.1) Satellite ----------
df <- read.csv("data/output/shipProc.csv", sep = ";")
# filter
df <- df %>% 
  filter(duplicated == FALSE) 
# filter(navigationStatus == "anchor")

# year
df$acquired <- as.POSIXct(df$acquired)
df$year <- year(df$acquired)
# day week (english)
Sys.setlocale("LC_TIME", "English")
df$wday <- lubridate::wday(df$acquired, label = TRUE, abbr = FALSE)

# records by weekday
df <- df %>%
  group_by(year, wday) %>%
  summarise(num_registros = n())
df <- na.omit(df)

# numeric sequentation of weekdays
df <- df %>%
  mutate(wday_num = case_when(
    wday == "Monday" ~ 1,
    wday == "Tuesday" ~ 2,
    wday == "Wednesday" ~ 3,
    wday == "Thursday" ~ 4,
    wday == "Friday" ~ 5,
    wday == "Saturday" ~ 6,
    wday == "Sunday" ~ 7
  ))

# as numeric
df$wday_num <- as.numeric(df$wday_num)
df$num_registros <- as.numeric(df$num_registros)


# 7.1.2) AIS data ---------------
ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# filter potential AIS position in the same timestamp
ais <- ais %>%
  distinct(mmsi, timestamp, .keep_all = TRUE)

# year
ais$timestamp <- as.POSIXct(ais$timestamp)
ais$year <- year(ais$timestamp)

# day week (english)
Sys.setlocale("LC_TIME", "English")
ais$wday <- lubridate::wday(ais$timestamp, label = TRUE, abbr = FALSE)

# records by weekday
ais <- ais %>%
  group_by(year, wday) %>%
  summarise(num_registros = n())

ais <- na.omit(ais)

# numeric sequentation of weekdays
ais <- ais %>%
  mutate(wday_num = case_when(
    wday == "Monday" ~ 1,
    wday == "Tuesday" ~ 2,
    wday == "Wednesday" ~ 3,
    wday == "Thursday" ~ 4,
    wday == "Friday" ~ 5,
    wday == "Saturday" ~ 6,
    wday == "Sunday" ~ 7
  ))

# as numeric
ais$wday_num <- as.numeric(ais$wday_num)
ais$num_registros <- as.numeric(ais$num_registros)


days <-  c("Monday", "Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday")

# 7.2 Plot
p7 <- ggplot() +
  # Satellite
  geom_point(data = df, aes(x = wday_num, y = num_registros), color = "#FF8828", size = 2, alpha = 0.35) +
  geom_smooth(data = df, aes(x = wday_num, y = num_registros), size = 1.15, span = 0.2, 
              color = "darkorange4", se = F, alpha = 0.1) +
  geom_smooth(data = df, aes(x = wday_num, y = num_registros), size = 0.75, span = 0.2, 
              color = "#FF8828", fill = "#F8D6BA") +
  # AIS
  geom_point(data = ais, aes(x = wday_num, y = num_registros), color = "deepskyblue3", size = 2, alpha = 0.35) +
  geom_smooth(data = ais, aes(x = wday_num, y = num_registros), size = 1.15, 
              color = "deepskyblue4", se = F, alpha = 0.1) +
  geom_smooth(data = ais, aes(x = wday_num, y = num_registros), size = 0.75, 
              color = "deepskyblue3", fill = "lightblue3") +
  # Fixed Camera
  
  # labels sequential days created
  scale_x_continuous(breaks = 1:length(days), labels = days) +
  # label text
  ylab("N.º Ships") + xlab("") +
  # theme
  theme_bw() +
  theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
        axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
        # axis labels
        axis.text.y = element_text(size = 9, family = "Arial"),
        axis.text.x = element_text(size = 8, family = "Arial",
                                   angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks = element_line(size = 0.4),
        axis.ticks.length = unit(-5, "pt"),  # negative lenght -> ticks inside the plot 
        # panel
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid = element_blank(),
  )

p7




