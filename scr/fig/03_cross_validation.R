

#------------------------------------------------------------------------------
# 03. Plot: Cross-validation differences between digitalizers
#------------------------------------------------------------------------------


# y = vessel lengt
# x = image 
# annotation = number of vessel per digitalizer


library(dplyr)
library(cowplot)
library(gghalves)
library(tidyverse)
library(gridExtra)
library(ggplot2)

# devtools::install_github('smin95/smplot2', force = TRUE)
library(smplot2)


# load data
df <- read.csv("data/output/cross_validation/shipProc.csv")

# prepare data
# rename item_ID
unique(df$item_id)

# change column values / rename imageID
df <- mutate(df, 
             item_id = case_when(
               item_id == '20200704_101623_1040' ~ 'Image 1',
               item_id == '20200721_080728_0f3c' ~ 'Image 2',
               item_id == '20220808_101214_46_2479' ~ 'Image 3',
               item_id == '20220816_093543_64_2451' ~ 'Image 4',
               item_id == '20220830_093538_35_2448' ~ 'Image 5'))

# as factor
df$digitizer <- as.factor(df$digitizer)
df$item_id <- as.factor(df$item_id)


# check digitizer names
unique(df$digitizer)
# Two datasets for each digitizer
df_jmb <- filter(df, digitizer == "JMB")
df_mst <- filter(df, digitizer == "MST")


# change column values / rename imageID
df_jmb <- mutate(df_jmb, 
             digitizer = case_when(
               digitizer == 'JMB' ~ 'Digitizer 2'))

df_mst <- mutate(df_mst, 
                 digitizer = case_when(
                   digitizer == 'MST' ~ 'Digitizer 1'))
             

# extract numebr of vessel by image and digitalizer for plot annotates
# digitilizer 2
ships_jmb <- df_jmb %>%
  group_by(item_id) %>% 
  summarise(count = n())
ships2 <- unique(ships_jmb$count)

ships_mst <- df_mst %>%
  group_by(item_id) %>%
  summarise(count = n())
ships1 <- unique(ships_mst$count)



# plot color palette
sm_palette(20)

# plot
pp <- ggplot(data = df, mapping = aes(x = item_id, y = length_m, fill = digitizer)) +
  
  sm_raincloud(data = df_jmb, which_side = "right",
               show.legend = FALSE,
               position = position_nudge(x = +0.15),
               point.params = list(size = 3, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0.6,
                                   position = sdamr::position_jitternudge(nudge.x = 0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_mst, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               violin.params = list(size = 10),
               point.params = list(size = 3, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0.6,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +

  # scale
  scale_y_continuous(limits = c(7, 50)) + # max-min values of length
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('Ship length (m)') +
  # theme
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, family = 'Arial'),
        axis.text.y = element_text(size = 11, family = 'Arial'),
        axis.text.x = element_text(size = 9, family = "Arial", color = "grey65"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.10,0.88),
        legend.title = element_blank(),
        legend.text = element_text(size = 9)
        )

# check
pp

# annotate 
p <- pp + 
  
  annotate("text", x = 0.90, y = 35.8, label = expression("N.^o ~ Ships"), parse = TRUE, 
           family = "Arial", size = 3, color = "grey75") +
  # img 1
  annotate("text", x = 0.85, y = 34, label = paste0("italic(n) == ", ships1[1]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "skyblue4") +
  annotate("text", x = 1.15, y = 34, label = paste0("italic(n) == ", ships2[1]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "#CD6600") +
  # img 2
  annotate("text", x = 1.85, y = 37, label = paste0("italic(n) == ", ships1[2]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "skyblue4") +
  annotate("text", x = 2.20, y = 37, label = paste0("italic(n) == ", ships2[2]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "#CD6600") +
  # img 3
  annotate("text", x = 2.85, y = 34, label = paste0("italic(n) == ", ships1[3]), parse = TRUE, 
         family = "Arial", size = 3.5, color = "skyblue4") +
  annotate("text", x = 3.20, y = 34, label = paste0("italic(n) == ", ships2[3]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "#CD6600") +
  # img 4
  annotate("text", x = 3.85, y = 34, label = paste0("italic(n) == ", ships1[4]), parse = TRUE, 
         family = "Arial", size = 3.5, color = "skyblue4") +
  annotate("text", x = 4.20, y = 34, label = paste0("italic(n) == ", ships2[4]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "#CD6600") +
  # img 5
  annotate("text", x = 4.85, y = 34, label = paste0("italic(n) == ", ships1[5]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "skyblue4") +
  annotate("text", x = 5.20, y = 34, label = paste0("italic(n) == ", ships2[5]), parse = TRUE, 
           family = "Arial", size = 3.5, color = "#CD6600")

p


# export save plot
p_png <- "fig/sup_fig1.png"
p_svg <- "fig/sup_fig1.svg"
ggsave(p_png, p, width=20, height=12, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=20, height=12, units="cm", dpi=350, bg="white")










# ------------------------------------------------------------------------------
# Supplementaty figure Scatter plot digitizers



pairs <- read.csv("data/output/cross_validation/length_dig1_dig2_pairs.csv")
# 184

# filter neighbors with >= 50m distance
pairs$dist <- as.numeric(pairs$dist)
# pairs <- pairs %>% filter(dist <= 10)
# pairs <- pairs %>% filter(digitizer1 != digitizer2)

# Calcular el RMSE entre las longitudes estimadas por los dos digitalizadores
rmse <- sqrt(mean((pairs$dig1_length - pairs$dig2_length)^2))

cat("RMSE:", rmse, "\n")

pearson_correlation <- cor(pairs$dig1_length, pairs$dig2_length)

# Mostrar el resultado
print(rmse)
print(pearson_correlation)
summary(pairs$dist)

# t-student
# Differences between number of vessel identified by method per scene
t_test <- t.test(pairs$dig1_length, pairs$dig2_length, paired = TRUE)
print(t_test)
# t = -4.7156, df = 183, p-value = 4.765e-06

summary(pairs$dig1_length)
sd(pairs$dig1_length)
summary(pairs$dig2_length)
sd(pairs$dig2_length)


dig1_length <- as.numeric(pairs$dig1_length)
dig2_length <- as.numeric(pairs$dig2_length)


# statistics models 
model <- lm(dig1_length ~ dig2_length, data = pairs) # Modelo de regresión lineal

# slope and intersects of the line
intercept <- round(coef(model)[1], 2)
slope <- round(coef(model)[2], 2)
r_squared <- round(summary(model)$r.squared, 2)


# Pearson correlation
pearson_corr <- cor(pairs$dig1_length, pairs$dig2_length, method = "pearson")
print(pearson_corr)

p <- ggplot(pairs, aes(x = dig1_length, y = dig2_length)) +
        geom_point(size = 2.5, color = "darkorange", fill = 'orange',alpha = 0.35, ) + 
        labs(x = "Vessel lengths (Analyst 1)",
             y = "Vessel lengths (Analyst 2)") +
        # axys limit
        ylim(5, 50) +
        # tendence line
        stat_smooth(method = "lm", col = "sienna3",) +  # Añadir la línea de tendencia
        # statistics
        annotate("text", x = 16, y = 46, 
          label = paste0("italic(y) == ", round(intercept, 2), " + ", round(slope, 2), " * italic(x) * ' \\n '"),
          parse = TRUE,  # Activa el formato matemático
          color = "grey40", size = 2.5) +
          # Pearson
        annotate("text", x = 16, y = 44, 
          label = paste0("Pearson correlation = ", round(pearson_corr, 2)),
          color = "grey40", size = 2.5) +
        # theme
        theme_bw() +
        theme(axis.title.x = element_text(family = "Arial", size = 9, colour = "grey35"),
              axis.title.y = element_text(family = "Arial", size = 9, colour = "grey35"),
              # axis labels
              axis.text.y = element_text(size = 10, family = "Arial"),
              axis.text.x = element_text(size = 10, family = "Arial"),
              axis.ticks = element_line(size = 0.6),
              axis.ticks.length = unit(-6, "pt"),  # negative lenght -> ticks inside the plot 
              # panel
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              # legend
              legend.position = "None")

p



# export
p_png <- "fig/sup_fig_digit_scatter_plot.png"
p_svg <- "fig/sup_fig_digit_scatter_plot.svg"
ggsave(p_png, p, width = 10, height = 12, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width = 10, height = 12, units="cm", dpi=350, bg="white")



