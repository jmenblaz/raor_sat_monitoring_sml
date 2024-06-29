# ---------------------------------------------------------------------------
# 05 - AIS and satellite difference statistics analysis
# ----------------------------------------------------------------------

library(dplyr)
library(tidyr)

ais <- read.csv("data/output/ais_scenes_interpolated.csv")
# filter ships with lenght == 0
ais <- ais %>% filter(length != 0)

# ship digitized
ships <- read.csv("data/output/shipProc.csv", sep = ";")
ships <- ships %>% filter(duplicated == "FALSE") # filter ships duplicated

# values min-max lengt
sat_length <- range(ships$length_m)
ais_length <- range(ais$length)
print(sat_length)
print(ais_length)


# extract info and combien df
ais <- ais %>% select(img_ID, length)
ships <- ships %>% 
  select(item_id, length_m) %>%
  rename(img_ID = item_id,
         length = length_m)

ais$method = "ais"
ships$method = "sat"

df <- rbind(ais,ships)
df$length <- as.numeric(df$length)
df$method <- as.factor(df$method)


# Prepare data for analysis
df <- df %>% group_by(img_ID, method) %>%
  summarize(n = n(),
            length = median(length))

df_wide <- df %>%
  pivot_wider(names_from = method, values_from = c(n, length))
# Note: t-student for more than 2 methods. For >2 use ANOVA

# using only image with data for both method...
df_wide <- na.omit(df_wide) 
# in total 93 images in wich are data from AIS and Satellite method# t-student for length by digitilizer
# of 174 scenes with ships digitalized


t_test_length <- t.test(df_wide$length_ais, df_wide$length_sat, paired = TRUE)
print(t_test_length)


# of 174 scenes with ships digitalized
t_test_length <- t.test(df_wide$n_ais, df_wide$n_sat, paired = TRUE)
print(t_test_length)







# Potential plot
# Crear un dataframe para las diferencias
length_diff <- data.frame(
  difference = df_wide$length_ais - df_wide$length_sat
)


# Crear el gráfico
ggplot(length_diff, aes(x = difference)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribución de las Diferencias en la Longitud de los Barcos",
    x = "Diferencia en la Longitud (AIS - SAT)",
    y = "Frecuencia"
  ) +
  theme_minimal()


