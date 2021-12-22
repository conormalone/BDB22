plays <- read.csv("plays.csv")
players <- read.csv("players.csv")
games <- read.csv("games.csv")
tracking18 <- read.csv("tracking2018.csv")
tracking19 <- read.csv("tracking2019.csv")
tracking20 <- read.csv("tracking2020.csv")
#compile tracking and delete originals
all_tracking <- rbind(tracking18, tracking19, tracking20)
rm(tracking18)
rm(tracking19)
rm(tracking20)
#add factors
plays$specialTeamsPlayType <- as.factor(plays$specialTeamsPlayType)
plays$specialTeamsResult <- as.factor(plays$specialTeamsResult)
plays$gameId <- as.factor(plays$gameId)
plays$playId <- as.factor(plays$playId)
plays$returnerId <-sub("\\;.*", "", plays$returnerId)
games$gameId <- as.factor(games$gameId)
all_tracking$gameId <- as.factor(all_tracking$gameId)
all_tracking$playId <- as.factor(all_tracking$playId)
all_tracking$event <- as.factor(all_tracking$event)
#add game data to plays
plays_and_games <- merge(x = plays, y = games, by = "gameId", all.x=TRUE)
#isolate gameId and Season for later
game_and_season <- games %>% dplyr::select(gameId, season) %>% unique()

rm(plays)
rm(games)
#add combined Id to give each play a unique reference
plays_and_games$comb_id <- paste0(as.character(plays_and_games$gameId), " ", as.character(plays_and_games$playId))
all_tracking$comb_id <- paste0(as.character(all_tracking$gameId), " ", as.character(all_tracking$playId))

#subset just kick returns, kickoff and punt, not onside kicks
return_plays <- plays_and_games %>% filter(specialTeamsResult == "Return") %>% drop_na(returnerId)
rm(plays_and_games)
#subset just tracking relating to these return plays  
return_tracking <- all_tracking[all_tracking$comb_id %in% return_plays$comb_id,]
example_play_sub <- merge(return_tracking, return_plays, by = "comb_id")

example_play <- example_play_sub %>%  filter(comb_id =="2018102805 2014")
just_our_guy <- example_play %>% filter(jerseyNumber == 51)
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
cols_fill <- c("#002244", "#663300", "#E31837")
cols_col <- c("#000000", "#663300", "#000000")
size_vals <- c(6, 4, 6)
shape_vals <- c(21, 16, 21)
plot_title <- example_play$playDescription[1]
nFrames <- max(example_play$frameId)

#plotting
anim <- ggplot() +
  
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 122) +
  
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
  geom_point(data = just_our_guy, aes(x = x,
                                      y = y, 
                                      shape = team,
                                      fill = team,
                                      group = nflId,
                                      colour = team),size =10)+
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
