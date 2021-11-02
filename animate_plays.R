
example_play <- merge(return_tracking, return_plays, by = "comb_id")

example_play <- example_play %>%  filter(comb_id =="2018090900 4236")

library(gganimate)
library(tidyverse)
library(gganimate)
library(cowplot)
library(ggridges)
library(repr)
source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
#upright dimensions
uprightLength = 18.5/3
uprightYardline = 120
uprightAccrossFieldLocation = 160/6
uprightColor = "#E8DE35"
uprightlineWidth = 2
uprightShape = 21
uprightSize = 4
uprightOutlineColor = 'black'

#attributes used for plot. first is away, second is football, third is home.
cols_fill <- c("dodgerblue1", "#663300", "firebrick1")
cols_col <- c("#000000", "#663300", "#000000")
size_vals <- c(6, 4, 6)
shape_vals <- c(21, 16, 21)
plot_title <- example_play$playDescription[1]
nFrames <- max(example_play$frameId)

#plotting
anim <- ggplot() +
  
  
  #creating field underlay
  gg_field(yardmin = 65, yardmax = 122) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = 'forestgreen',
                                        color = 'forestgreen'),
        panel.grid = element_blank()) +
  
  
  #adding field goal uprights
  annotate(geom = 'segment',
           x = uprightYardline,
           xend = uprightYardline,
           y = uprightAccrossFieldLocation + uprightLength/2,
           yend = uprightAccrossFieldLocation - uprightLength/2,
           color = uprightColor,
           lwd = uprightlineWidth) +
  
  annotate(geom = 'point',
           x = uprightYardline,
           y = uprightAccrossFieldLocation + uprightLength/2,
           size = uprightSize,
           shape = uprightShape,
           fill = uprightColor,
           color = uprightOutlineColor) +
  
  annotate(geom = 'point',
           x = uprightYardline,
           y = uprightAccrossFieldLocation - uprightLength/2,
           size = uprightSize,
           shape = uprightShape,
           fill = uprightColor,
           color = uprightOutlineColor) +
  
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = FALSE) + 
  scale_shape_manual(values = shape_vals, guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  
  
  #adding players
  geom_point(data = example_play, aes(x = x,
                                      y = y, 
                                      shape = team,
                                      fill = team,
                                      group = nflId,
                                      size = team,
                                      colour = team), 
             alpha = 0.7) +  
  
  #adding jersey numbers
  geom_text(data = example_play,
            aes(x = x, y = y, label = jerseyNumber),
            colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL 


#saving animation to display in markdown cell below:
anim_save('LowestOffsetAnimation.gif',
          animate(anim, width = 720, height = 440,
                  fps = 10, nframe = nFrames))