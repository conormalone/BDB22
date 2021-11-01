library(tidyverse)
#import files needed now and set factors
plays <- read.csv("plays.csv")
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
games$gameId <- as.factor(games$gameId)
all_tracking$gameId <- as.factor(all_tracking$gameId)
all_tracking$playId <- as.factor(all_tracking$playId)
all_tracking$event <- as.factor(all_tracking$event)
#add game data to plays
plays_and_games <- merge(x = plays, y = games, by = "gameId", all.x=TRUE)
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
rm(all_tracking)
#combine game and play dfs

#add returner to tracking
return_tracking <-merge(x = return_tracking, y = return_plays[ ,c("comb_id", "kickReturnYardage", "returnerId", "possessionTeam","homeTeamAbbr")], by = "comb_id", all.x=TRUE)


#preprocessing to add some useful variables
pre_processing_function <- function(df){
  dataframe <- df %>% 
    mutate(toLeft = playDirection == "left", 
           isBallCarrier = nflId == returnerId,
           teamOnOffense = ifelse(possessionTeam == homeTeamAbbr, "home", "away"),  
           isOnOffense = team == teamOnOffense,  ## Is player on offense?
           x_std = ifelse(toLeft, 120-x, x) - 10, ## Standardizes X
           y_std = ifelse(toLeft, 160/3-y, y) ## Standardized Y
           )   ## Standardized Dir
  
  
  return(dataframe)
}

dataframe<-pre_processing_function(return_tracking)

##########################
#decide how to subset the data and put that here
#get returner dist to other players ahead of him for each frame, take frames where dist is within say 5 yards
#from original BDB20
library(sp)
library(raster)
#train_1<-dataframe %>% tidyr::fill(c(comb_id, frameId,  x_std, y_std))
#get location of ball/rusher
ballocation<-dataframe %>% filter(nflId == returnerId) %>% dplyr::select(c(comb_id, frameId, x_std, y_std))
#join balllocation x,y as new variables
IPWorking<-dataframe %>% dplyr::filter(isOnOffense ==F & team != "football")%>%
  left_join(ballocation, by = c("comb_id", "frameId"))
linesstore<-NULL
#for each row calculate distance from ball                     
linesstore<-apply(IPWorking, 1, function(x) dist(rbind(c(x["x_std.x"], x["y_std.x"]), c(x["x_std.y"], x["y_std.y"])), method = "euclidean" )[1])
#store distance from ball as new variable
IPWorking$linelength<-linesstore 
#subset for defense and order by distance from rusher
IPWorking_Def<-IPWorking %>%
  group_by(comb_id, frameId) %>%
    mutate(lineorder = order(order(linelength, decreasing=F)), returner_x = x_std.x) %>% 
    dplyr::select("comb_id", "frameId", "lineorder", "x" ) %>% 
    pivot_wider(names_from = lineorder, values_from = x)%>%
    left_join(ballocation, by = c("comb_id", "frameId"))

reception <- c("kick_received", "pass_outcome_caught", "punt_received", "kick_recovered" )
play_over <- c("out_of_bounds", "tackle", "fumble_offense_recovered", "touchdown", "punt_downed", "first_contact", "safety")

#filter df so it only has events contained in snap/forward passes/sacks
df_merged <- dataframe %>% mutate(play_type = case_when(
  event %in% reception ~ "received",
  event %in% play_over ~ "play_over"))
df_merged <- df_merged %>% filter(!is.na(play_type))
df_smaller <-distinct(df_merged, gameId, playId, frameId, .keep_all = TRUE)%>% 
  dplyr::select(gameId, playId, frameId, play_type )
df_smaller <- df_smaller %>% group_by(gameId, playId) %>%
  pivot_wider(id_cols = c(gameId, playId),names_from = play_type, values_from = frameId, values_fn = min)

#remove instances without both a reception and an end event
df_smaller <- df_smaller[rowSums(is.na(df_smaller))<1,]

# have wide frame of defender x pos
############
#end bdb20 code







#code to put in graph format, ties up with python file.
to_loop <- dataframe %>% dplyr::select(comb_id, kickReturnYardage, x, y) %>% 
  group_by(PlayId) %>% 
  mutate(row = row_number())

yard_loop <- to_loop %>% dplyr::select(comb_id, kickReturnYardage) %>% group_by(comb_id) %>% 
  summarise(Yards = min(kickReturnYardage))  
#loop to get distances between all points (adjacency matrix) (graph.a)
distance_matrix <- list()  
froot_loop <- to_loop %>% 
  dplyr::select(comb_id, x,y, row) %>% 
  pivot_wider( names_from = row, values_from = c(x,y))
for(i in 1:nrow(froot_loop)){
  points <- cbind(c(froot_loop$X_1[i],froot_loop$X_2[i],froot_loop$X_3[i],froot_loop$X_4[i],froot_loop$X_5[i],froot_loop$X_6[i],froot_loop$X_7[i],froot_loop$X_8[i],froot_loop$X_9[i],froot_loop$X_10[i],froot_loop$X_11[i],froot_loop$X_12[i],froot_loop$X_13[i],froot_loop$X_14[i],froot_loop$X_15[i],froot_loop$X_16[i],froot_loop$X_17[i],froot_loop$X_18[i],froot_loop$X_19[i],froot_loop$X_20[i],froot_loop$X_21[i],froot_loop$X_22[i]),
                  c(froot_loop$Y_1[i],froot_loop$Y_2[i],froot_loop$Y_3[i],froot_loop$Y_4[i],froot_loop$Y_5[i],froot_loop$Y_6[i],froot_loop$Y_7[i],froot_loop$Y_8[i],froot_loop$Y_9[i],froot_loop$Y_10[i],froot_loop$Y_11[i],froot_loop$Y_12[i],froot_loop$Y_13[i],froot_loop$Y_14[i],froot_loop$Y_15[i],froot_loop$Y_16[i],froot_loop$Y_17[i],froot_loop$Y_18[i],froot_loop$Y_19[i],froot_loop$Y_20[i],froot_loop$Y_21[i],froot_loop$Y_22[i]))
  distance_result <- as.matrix(pointDistance(points,points, lonlat=F, allpairs =T))
  distance_matrix[[i]] <-distance_result
}

#get node features(graph.x)
#get S and R D B for every play (2 node features)
features_set <- dataframe %>% mutate(node_type = case_when(
  isBallCarrier ~ "R",
  isOnOffense ~ "B",
  isOnOffense ==F ~ "D")) %>% 
  dplyr::select(comb_id, s, node_type)
features_set$node_type <- as.factor(features_set$node_type)
library(mltools)
library(data.table)
features_set <-one_hot(data.table(features_set),cols = "node_type")


features_list <- list()  
for(i in 1:length(levels(features_set$comb_id))){
  features_list[[i]] <- features_set[features_set$PlayId == levels(features_set$comb_id)[i],c(2:5)]
}

YardList <- yard_loop$Yards
     