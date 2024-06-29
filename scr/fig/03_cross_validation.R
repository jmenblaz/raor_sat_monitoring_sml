

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
  
  sm_raincloud(data = df_jmb,
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







