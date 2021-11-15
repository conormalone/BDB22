library(tidyverse)
set.seed(1234)
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
return_tracking <-merge(x = return_tracking, y = return_plays[ ,c("comb_id", "kickReturnYardage", "returnerId", "possessionTeam","homeTeamAbbr", "specialTeamsPlayType")], by = "comb_id", all.x=TRUE)


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
#get returner dist to other players ahead of him for each frame,
#from original BDB20
library(sp)
library(raster)
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
#subset for defense and order by distance from returner
wide_dist_to_returner<-IPWorking %>%
  group_by(comb_id, frameId) %>%
    mutate(lineorder = order(order(linelength, decreasing=T))) %>% 
    dplyr::select("comb_id", "frameId", "lineorder", "nflId" ) %>% 
    pivot_wider(names_from = lineorder, values_from = nflId)%>%
    left_join(ballocation, by = c("comb_id", "frameId"))


#sort events into reception (start) and play_over (end) to only examine relevant frames
reception <- c("kick_received", "pass_outcome_caught", "punt_received", "kick_recovered" )
play_over <- c("out_of_bounds", "tackle", "fumble_offense_recovered", "touchdown", "punt_downed", "first_contact", "safety")

#filter df so it only has events contained in reception/playover
df_merged <- dataframe %>% mutate(play_type = case_when(
  event %in% reception ~ "received",
  event %in% play_over ~ "play_over"))
df_merged <- df_merged %>% filter(!is.na(play_type))
df_in_play_long <-distinct(df_merged, comb_id, frameId, .keep_all = TRUE)%>% 
  dplyr::select(comb_id, frameId, play_type )
df_in_play <- df_in_play_long %>% group_by(comb_id) %>%
  pivot_wider(id_cols = c(comb_id),names_from = play_type, values_from = frameId, values_fn = max)

#remove instances without both a reception and an end event
df_in_play <- df_in_play[rowSums(is.na(df_in_play))<1,]



#get location of returner to get total return distance to go
df_play_dist <- df_in_play_long  %>% left_join(ballocation, by = c("comb_id", "frameId"))%>% 
  group_by(comb_id) %>% #filter(play_type == "received")%>%
  pivot_wider(id_cols = c(comb_id),names_from = play_type, values_from = x_std, values_fn = min) #%>% 
  #mutate(play_dist = ceiling(received - play_over))



#set combids as factors
df_in_play$comb_id <- as.factor(df_in_play$comb_id)
wide_dist_to_returner$comb_id <- as.factor(wide_dist_to_returner$comb_id)
#loop to subset the nearest player matrix to just between reception and tackle
nearest_player_reception_to_tackle <- data.frame()
result <- data.frame()

for(i in 1:length(levels(df_in_play$comb_id))){
  wide_dist_to_returner_just_combid <- wide_dist_to_returner[wide_dist_to_returner$comb_id ==levels(df_in_play$comb_id)[i],] 
  df_in_play_just_combid <- df_in_play[df_in_play$comb_id == levels(df_in_play$comb_id)[i],] 
  result <- wide_dist_to_returner_just_combid %>% 
    filter(comb_id == df_in_play_just_combid$comb_id && frameId >= df_in_play_just_combid$received && frameId <= df_in_play_just_combid$play_over)
  
  nearest_player_reception_to_tackle <- rbind(nearest_player_reception_to_tackle, result)
} 
# have wide frame of defender x pos
#get frames where kicking team players are packed
nearest_player_reception_to_tackle$lag <- lag(nearest_player_reception_to_tackle$`1`)
nearest_player_reception_to_tackle$diff <- nearest_player_reception_to_tackle$lag != nearest_player_reception_to_tackle$`1`

packed_frames <- nearest_player_reception_to_tackle %>% filter(diff) %>% dplyr::select(comb_id, frameId)

df_in_play_renamed <-rename(df_in_play, frameId = received) 
packed_frames_inc_start <- unique(rbind(packed_frames,df_in_play_renamed[c("comb_id", "frameId")] ))
droplevels(packed_frames_inc_start$comb_id)  
length(levels(packed_frames_inc_start$comb_id))

#now to test / train split and add punt/ko to features

#split on combid so same plays aren't in test and train
N<-length(levels(packed_frames_inc_start$comb_id))
trainset<-sort(sample(1:N,size=floor(N*0.70)))
nottestset<-setdiff(1:N,trainset)
validset<-sort(sample(nottestset,size=length(nottestset)/2))
testset<-sort(setdiff(nottestset,validset))
train_comb_id <-levels(packed_frames_inc_start$comb_id)[trainset]
val_comb_id <-levels(packed_frames_inc_start$comb_id)[validset]
test_comb_id <-levels(packed_frames_inc_start$comb_id)[testset]
#get split in dataframe
train_data <-dataframe %>% filter(comb_id %in% train_comb_id)
val_data <-dataframe %>% filter(comb_id %in% val_comb_id)
test_data <-dataframe %>% filter(comb_id %in% test_comb_id)















#code to put in graph format, ties up with python file.
library(mltools)
library(data.table)
graph_processing_function <- function(dataframe){
df <- dataframe %>% filter(displayName != "football") %>% 
  left_join(ballocation, by = c("comb_id", "frameId"))%>%
  left_join(df_play_dist, by = c("comb_id"))%>%
  unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE)%>%
  mutate(comb_and_frame = as.factor(comb_and_frame), returnYardstoGo = floor(x_std.y - play_over))
function_graph_list <- list()
to_loop <- df %>% dplyr::select(comb_and_frame, returnYardstoGo, x, y) %>% 
  group_by(comb_and_frame) %>% 
  mutate(row = row_number())

yard_loop <- to_loop %>% dplyr::select(comb_and_frame, returnYardstoGo) %>% group_by(comb_and_frame) %>% 
  summarise(Yards = min(returnYardstoGo))  
#loop to get distances between all points (adjacency matrix) (graph.a)
distance_matrix <- list()  
froot_loop <- to_loop %>% 
  dplyr::select(comb_and_frame, x,y, row) %>% 
  pivot_wider( names_from = row, values_from = c(x,y))
for(i in 1:nrow(froot_loop)){
  points <- cbind(c(froot_loop$x_1[i],froot_loop$x_2[i],froot_loop$x_3[i],froot_loop$x_4[i],froot_loop$x_5[i],froot_loop$x_6[i],froot_loop$x_7[i],froot_loop$x_8[i],froot_loop$x_9[i],froot_loop$x_10[i],froot_loop$x_11[i],froot_loop$x_12[i],froot_loop$x_13[i],froot_loop$x_14[i],froot_loop$x_15[i],froot_loop$x_16[i],froot_loop$x_17[i],froot_loop$x_18[i],froot_loop$x_19[i],froot_loop$x_20[i],froot_loop$x_21[i],froot_loop$x_22[i]),
                  c(froot_loop$y_1[i],froot_loop$y_2[i],froot_loop$y_3[i],froot_loop$y_4[i],froot_loop$y_5[i],froot_loop$y_6[i],froot_loop$y_7[i],froot_loop$y_8[i],froot_loop$y_9[i],froot_loop$y_10[i],froot_loop$y_11[i],froot_loop$y_12[i],froot_loop$y_13[i],froot_loop$y_14[i],froot_loop$y_15[i],froot_loop$y_16[i],froot_loop$y_17[i],froot_loop$y_18[i],froot_loop$y_19[i],froot_loop$y_20[i],froot_loop$y_21[i],froot_loop$y_22[i]))
  distance_result <- as.matrix(pointDistance(points,points, lonlat=F, allpairs =T))
  distance_matrix[[i]] <-distance_result
}

#get node features(graph.x)
#get S and R D B for every play (2 node features)
features_set <- df %>% mutate(node_type = case_when(
  isBallCarrier ~ "R",
  isOnOffense ~ "B",
  isOnOffense ==F ~ "D")) %>% 
  dplyr::select(comb_and_frame, s, node_type, specialTeamsPlayType)
features_set$node_type <- as.factor(features_set$node_type)
features_set$specialTeamsPlayType <- droplevels(features_set$specialTeamsPlayType)
features_set <-one_hot(data.table(features_set),cols = c("node_type", "specialTeamsPlayType"))


features_list <- list()  
for(i in 1:length(levels(features_set$comb_and_frame))){
  features_list[[i]] <- features_set[features_set$comb_and_frame == levels(features_set$comb_and_frame)[i],c(2:7)]
}

YardList <- yard_loop$Yards
function_graph_list <-list(y = YardList, x = features_list, a = distance_matrix)

return(function_graph_list)
} 
test <-graph_processing_function(test_data)     
train <-graph_processing_function(train_data)     
validate <-graph_processing_function(val_data)     

library(reticulate)
source_python("python_load_and_train_graph.py")


predictions <- py$model$predict(loader_all$load(), steps = loader_all$steps_per_epoch)
