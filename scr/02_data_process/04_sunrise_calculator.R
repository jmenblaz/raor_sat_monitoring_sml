# -----------------------------------------------------------------------------
# 04 - Sunrise calculator
# -----------------------------------------------------------------------------
 
# Instalar y cargar 
# install.packages("suncalc")
# install.packages("lubridate")

library(suncalc)
library(lubridate)
library(dplyr)
library(sf)
library(ggplot2)

# Coordenadas de la Bahía de Palma en Mallorca
latitude <- 39.2529
longitude <- 2.4435
# Vector de años desde 2012 hasta 2022
años <- 2016:2023

# Crear un data frame para almacenar la información
sunrise_data <- data.frame(Fecha = as.Date(character()), Amanecer = character(), Amanecer_UTC = character(), stringsAsFactors = FALSE)

# Iterar sobre todos los años y fechas
for (año in años) {
  # Vector de fechas desde el 25 de agosto hasta el 20 de septiembre del año actual
  fechas <- seq(as.Date(paste(año, "-08-25", sep = "")), as.Date(paste(año, "-09-20", sep = "")), by = "days")
  
  # Iterar sobre todas las fechas y almacenar la información
  for (f in 1:length(fechas)) {
    fecha <- fechas[f]
    # fecha <- as.Date(fecha)
    sun_times <- getSunlightTimes(fecha, lat = latitude, lon = longitude)
    sunrise_local <- sun_times$sunrise
    
    # Convertir la hora local a UTC utilizando lubridate
    sunrise_utc <- force_tz(sunrise_local, tzone = "Europe/Madrid") %>% with_tz("UTC")
    
    # Almacenar la información en el data frame
    sunrise_data <- rbind(sunrise_data, data.frame(date = fecha, sunrise = format(sunrise_local, "%H:%M:%S"), sunrise_UTC = format(sunrise_utc, "%H:%M:%S")))
  }
}

# combine date and time -> sunrise timestamp
sunrise_data$sunrise_timestamp_utc <- as.POSIXct(paste(sunrise_data$date, sunrise_data$sunrise_UTC), format="%Y-%m-%d %H:%M:%S", tz="UTC")

#Export .CSV
write.csv(sunrise_data, "data/sunrise/sunrise_data.csv", row.names = FALSE)


##CALCULAR LA HORA RELATIVA DE NUESTRAS IMAGENES = MINUTOS TRAS EL AMANECER
# Load image time data
image_data <- read_sf("data/output/scene_ais_data.gpkg")
sunrise_data <- read.csv("data/sunrise/sunrise_data.csv")

# same names of the variables between df
image_data$date <- as.Date(image_data$acquired_UTC)
sunrise_data$date <- as.Date(sunrise_data$date)

# merge / join
sunrise_image_data <- merge(image_data, sunrise_data, by.x = "date", by.y = "date", all.x = TRUE)

sunrise_image_data$acquired_UTC <- as.POSIXct(sunrise_image_data$acquired_UTC, format="%Y-%m-%d %H:%M:%S", tz="UTC")
sunrise_image_data$sunrise_timestamp_utc <- as.POSIXct(sunrise_image_data$sunrise_timestamp_utc, format="%Y-%m-%d %H:%M:%S", tz="UTC")
# time difference
sunrise_image_data$min_from_sunrise <- as.numeric(difftime(sunrise_image_data$acquired_UTC, sunrise_image_data$sunrise_timestamp_utc, units = "mins"))


# remove geomtry
scene_data_nogeo <- st_drop_geometry(sunrise_image_data)

#Export .CSV
write.csv(sunrise_image_data, "data/output/sunrise_data/sunrise_image_data.csv", row.names = FALSE)
# No geo
write.csv(scene_data_nogeo, "data/output/scene_data.csv", row.names = FALSE)
# sptial data
st_write(sunrise_image_data, "data/output/scene_ais_data.gpkg", append = FALSE)












