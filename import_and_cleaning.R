library(tidyverse)
library(sp)
library(raster)
library(mltools)
library(data.table)
set.seed(1234)
#import files needed now and set factors
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
fc_plays <- plays_and_games %>% filter(specialTeamsResult == "Fair Catch") %>% drop_na(returnerId)
rm(plays_and_games)
#subset just tracking relating to these return plays  
return_tracking <- all_tracking[all_tracking$comb_id %in% return_plays$comb_id,]
fc_tracking <- all_tracking[all_tracking$event == "fair_catch",]
rm(all_tracking)
#combine game and play dfs

#add returner to tracking
return_tracking <-merge(x = return_tracking, y = return_plays[ ,c("comb_id", "kickReturnYardage", "returnerId", "possessionTeam","homeTeamAbbr", "specialTeamsPlayType")], by = "comb_id", all.x=TRUE)
fc_tracking <-merge(x = fc_tracking, y = fc_plays[ ,c("comb_id", "kickReturnYardage", "returnerId", "possessionTeam","homeTeamAbbr", "specialTeamsPlayType")], by = "comb_id", all.x=TRUE)

#preprocessing to add some useful variables
pre_processing_function <- function(df_input){
  dataframe <- df_input %>% 
    mutate(toLeft = playDirection == "left", 
           isBallCarrier = nflId == returnerId,
           teamOnOffense = ifelse(possessionTeam == homeTeamAbbr, "home", "away"),  
           isOnOffense = team == teamOnOffense,  ## Is player on offense?
           x_std = ifelse(toLeft, 120-x, x) - 10, ## Standardizes X
           y_std = ifelse(toLeft, 160/3-y, y) ## Standardized Y
    )   ## Standardized Dir
  
  
  return(dataframe)
}
play_pre_processed_dataframe<-pre_processing_function(return_tracking)
fc_pre_processed_dataframe<-pre_processing_function(fc_tracking)
#clean up to make less cpu intensive on Kaggle
rm(return_tracking)
dataframe <- play_pre_processed_dataframe %>% dplyr::select(-c("toLeft", "jerseyNumber", "position", "homeTeamAbbr","time","dis","teamOnOffense", "possessionTeam", "playDirection"))
rm(play_pre_processed_dataframe)
fc_dataframe <- fc_pre_processed_dataframe %>% dplyr::select(-c("toLeft", "jerseyNumber", "position", "homeTeamAbbr","time","dis","teamOnOffense", "possessionTeam", "playDirection"))
rm(fc_pre_processed_dataframe)
##########################
#get returner dist to other players ahead of him for each frame,
#from original BDB20

#get location of ball/rusher
ballocation <-dataframe %>% filter(nflId == returnerId) %>% dplyr::select(c(comb_id, frameId, x_std, y_std))
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

#get frames where kicking team players are packed (passed) to make snapshot
nearest_player_reception_to_tackle$lag <- lag(nearest_player_reception_to_tackle$`1`)
nearest_player_reception_to_tackle$diff <- nearest_player_reception_to_tackle$lag != nearest_player_reception_to_tackle$`1`

packed_frames <- nearest_player_reception_to_tackle %>% filter(diff) %>% dplyr::select(comb_id, frameId)
df_in_play_renamed <-rename(df_in_play, frameId = received) 
packed_frames_inc_start <- unique(rbind(packed_frames,df_in_play_renamed[c("comb_id", "frameId")] ))
packed_frames_inc_start$comb_id <- droplevels(packed_frames_inc_start$comb_id) 
packed_frames_list <-paste0(packed_frames_inc_start$comb_id," ", packed_frames_inc_start$frameId)

#split on combid so same plays aren't in test and train
N<-length(levels(packed_frames_inc_start$comb_id))
trainset<-sort(sample(1:N,size=floor(N*0.70)))
nottestset<-setdiff(1:N,trainset)
validset<-sort(sample(nottestset,size=length(nottestset)/2))
testset<-sort(setdiff(nottestset,validset))
test_rows <- packed_frames_inc_start$comb_id %in%packed_frames_inc_start$comb_id[testset]
train_rows <- packed_frames_inc_start$comb_id %in%packed_frames_inc_start$comb_id[trainset]
val_rows <- packed_frames_inc_start$comb_id %in%packed_frames_inc_start$comb_id[validset]


#subset dataframe to just chosen plays
dataframe <- dataframe %>% 
  filter(comb_id %in% levels(packed_frames_inc_start$comb_id) & frameId %in% packed_frames_inc_start$frameId)

rm(packed_frames_inc_start)

rm(IPWorking)
rm(df_merged)
rm(linesstore)
rm(wide_dist_to_returner)
rm(nearest_player_reception_to_tackle)

#function to put chosen data in graph format, for use with Python Spektral package
graph_processing_function <- function(dataframe, leave_one_out = 1){
  #get ball location for each play
  ball_location <-dataframe %>% filter(nflId == returnerId) %>% dplyr::select(c(comb_id, frameId, x_std, y_std))
  #join ball location and distance
  df <- dataframe %>% filter(displayName != "football") %>% 
    left_join(ball_location, by = c("comb_id", "frameId"))%>%
    left_join(df_play_dist, by = c("comb_id"))%>%
    #create new row id combining gameId, playId and frameId
    unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE)%>%
    #create returnyardstogo variable to get yards remaining on a play
    mutate(comb_and_frame = as.factor(comb_and_frame), returnYardstoGo = floor(x_std.y - play_over))
#get distance from ball to player on each frame
  linesstore<-apply(df, 1, function(x) dist(rbind(c(x["x_std.x"], x["y_std.x"]), c(x["x_std.y"], x["y_std.y"])), method = "euclidean" )[1])
  #store distance from ball as new variable
  df$linelength<-linesstore
rm(linesstore) 
rm(ball_location)
  #group combandframe get node feature details
  function_graph_list <- list()
  #if loo is selected subset on chosen frames, otherwise no need
  if(leave_one_out ==1){
  to_loop <- df %>% 
    group_by(comb_and_frame) %>% 
    mutate(node_type = case_when(
      isBallCarrier ~ "R",
      isOnOffense ~ "B",
      isOnOffense ==F ~ "D"), row = rank(linelength, ties.method= "random"))%>% 
    filter(comb_and_frame %in% packed_frames_list) %>% 
    dplyr::select(comb_and_frame, returnYardstoGo,nflId,isBallCarrier,isOnOffense, s, node_type, specialTeamsPlayType, x, y, row) 
  } else{to_loop <- df %>% 
    group_by(comb_and_frame) %>% 
    mutate(node_type = case_when(
      isBallCarrier ~ "R",
      isOnOffense ~ "B",
      isOnOffense ==F ~ "D"), row = rank(linelength, ties.method= "random"))%>% 
    #filter(comb_and_frame %in% packed_frames_list) %>% 
    dplyr::select(comb_and_frame, returnYardstoGo,nflId,isBallCarrier,isOnOffense, s, node_type, specialTeamsPlayType, x, y, row)}
  to_loop$comb_and_frame <- droplevels(to_loop$comb_and_frame)
  to_loop$node_type <- as.factor(to_loop$node_type)
  to_loop$specialTeamsPlayType <- droplevels(to_loop$specialTeamsPlayType)
  to_loop <-one_hot(data.table(to_loop),cols = c("node_type", "specialTeamsPlayType"))

rm(df)  
  
    #get y values
  yard_loop <- to_loop %>% dplyr::select(comb_and_frame, returnYardstoGo) %>% group_by(comb_and_frame) %>% 
    summarise(Yards = min(returnYardstoGo))  
  
  YardList <- yard_loop$Yards
  #catch any remaining errors, 
  YardList[is.na(YardList)] <- 0 
  
  #loop to get distances between all points (adjacency matrix) (graph.a)
    froot_loop <- to_loop %>% 
    dplyr::select(comb_and_frame, x,y, row) %>% 
    pivot_wider( names_from = row, values_from = c(x,y))
  #loop to match nflIds to loo matrices
  id_loop <- to_loop %>% filter(node_type_D ==1) %>% 
    dplyr::select(comb_and_frame, nflId, row) %>% 
    pivot_wider( names_from = row, values_from = nflId)
  blocker_loop<- to_loop %>% 
    dplyr::select(comb_and_frame, node_type_D, row) %>% filter(node_type_D ==1)
  #get node features(graph.x)
  #get S and R D B for every play (2 node features)
  dist_matrix <-apply(froot_loop, 1, function(x) pointDistance(cbind(c(x["x_1"] ,x["x_2"] ,x["x_3"] ,x["x_4"] ,x["x_5"] ,x["x_6"] ,x["x_7"] ,x["x_8"] ,x["x_9"] ,x["x_10"] ,x["x_11"] ,x["x_12"] ,x["x_13"] ,x["x_14"] ,x["x_15"] ,x["x_16"] ,x["x_17"] ,x["x_18"] ,x["x_19"] ,x["x_20"] ,x["x_21"] ,x["x_22"] ),
                                                                     c(x["y_1"] ,x["y_2"] ,x["y_3"] ,x["y_4"] ,x["y_5"] ,x["y_6"] ,x["y_7"] ,x["y_8"] ,x["y_9"] ,x["y_10"] ,x["y_11"] ,x["y_12"] ,x["y_13"] ,x["y_14"] ,x["y_15"] ,x["y_16"] ,x["y_17"] ,x["y_18"] ,x["y_19"] ,x["y_20"] ,x["y_21"] ,x["y_22"] )), cbind(c(x["x_1"] ,x["x_2"] ,x["x_3"] ,x["x_4"] ,x["x_5"] ,x["x_6"] ,x["x_7"] ,x["x_8"] ,x["x_9"] ,x["x_10"] ,x["x_11"] ,x["x_12"] ,x["x_13"] ,x["x_14"] ,x["x_15"] ,x["x_16"] ,x["x_17"] ,x["x_18"] ,x["x_19"] ,x["x_20"] ,x["x_21"] ,x["x_22"] ),
                                                                                                                                                                                                                                                                                                                         c(x["y_1"] ,x["y_2"] ,x["y_3"] ,x["y_4"] ,x["y_5"] ,x["y_6"] ,x["y_7"] ,x["y_8"] ,x["y_9"] ,x["y_10"] ,x["y_11"] ,x["y_12"] ,x["y_13"] ,x["y_14"] ,x["y_15"] ,x["y_16"] ,x["y_17"] ,x["y_18"] ,x["y_19"] ,x["y_20"] ,x["y_21"] ,x["y_22"] )), lonlat=F, allpairs =T))
  
  #loop to get adjacency matrix for leave-one-out (blocking) calculation
  loo_matrix <- list()  
  loo_feature_list <- list()
  loo_rusher <-data.frame()
  if(leave_one_out ==1){
  features_list <- by(to_loop, to_loop$comb_and_frame, function(x) dplyr::select(x,c("s","node_type_B","node_type_D","node_type_R","specialTeamsPlayType_Kickoff","specialTeamsPlayType_Punt")))  
  for(i in 1:nrow(froot_loop)){
    loo_matrix[[i]] <-list()
    loo_feature_list[[i]] <-list()
    who_are_blockers_df <- blocker_loop %>% filter(comb_and_frame== froot_loop$comb_and_frame[i]) %>% 
      dplyr::select(row)
    who_are_blockers_list <-as.vector(who_are_blockers_df$row)
    #for each blocker on play capture the feature list and adj matrix without them
    for(j in 1:length(who_are_blockers_list)){
      loo_rusher <-  rbind(loo_rusher, c(levels(id_loop$comb_and_frame)[i],id_loop[i, as.character(who_are_blockers_df[j]$row)][[1]], YardList[i]))
      loo_feature_list[[i]][[j]] <-features_list[[i]][-(who_are_blockers_list[j])]
      loo_matrix[[i]][[j]] <- matrix(dist_matrix[,i],22,22)
      loo_matrix[[i]][[j]] <-loo_matrix[[i]][[j]][-(who_are_blockers_list[j]),]
      loo_matrix[[i]][[j]] <-loo_matrix[[i]][[j]][,-who_are_blockers_list[j]]
    }
  }
  }
  #non loo details, for expected yards leave furthest row out so training is on 21 players  
  training_to_loop <- to_loop %>% filter(row !=22)
    training_froot_loop <- training_to_loop %>% 
      dplyr::select(comb_and_frame, x,y, row) %>% 
      pivot_wider( names_from = row, values_from = c(x,y))
    training_features_list_by <- by(training_to_loop, training_to_loop$comb_and_frame, function(x) dplyr::select(x,c("s","node_type_B","node_type_D","node_type_R","specialTeamsPlayType_Kickoff","specialTeamsPlayType_Punt")))
    training_features_list <- list()
    for(i in 1:nrow(training_froot_loop)){
      training_features_list[[i]] <-training_features_list_by[[i]]
    }
    
    training_dist_matrix <-apply(training_froot_loop, 1, function(x) pointDistance(cbind(c(x["x_1"] ,x["x_2"] ,x["x_3"] ,x["x_4"] ,x["x_5"] ,x["x_6"] ,x["x_7"] ,x["x_8"] ,x["x_9"] ,x["x_10"] ,x["x_11"] ,x["x_12"] ,x["x_13"] ,x["x_14"] ,x["x_15"] ,x["x_16"] ,x["x_17"] ,x["x_18"] ,x["x_19"] ,x["x_20"] ,x["x_21"]),
                                                                       c(x["y_1"] ,x["y_2"] ,x["y_3"] ,x["y_4"] ,x["y_5"] ,x["y_6"] ,x["y_7"] ,x["y_8"] ,x["y_9"] ,x["y_10"] ,x["y_11"] ,x["y_12"] ,x["y_13"] ,x["y_14"] ,x["y_15"] ,x["y_16"] ,x["y_17"] ,x["y_18"] ,x["y_19"] ,x["y_20"] ,x["y_21"] )), cbind(c(x["x_1"] ,x["x_2"] ,x["x_3"] ,x["x_4"] ,x["x_5"] ,x["x_6"] ,x["x_7"] ,x["x_8"] ,x["x_9"] ,x["x_10"] ,x["x_11"] ,x["x_12"] ,x["x_13"] ,x["x_14"] ,x["x_15"] ,x["x_16"] ,x["x_17"] ,x["x_18"] ,x["x_19"] ,x["x_20"] ,x["x_21"] ),

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       c(x["y_1"] ,x["y_2"] ,x["y_3"] ,x["y_4"] ,x["y_5"] ,x["y_6"] ,x["y_7"] ,x["y_8"] ,x["y_9"] ,x["y_10"] ,x["y_11"] ,x["y_12"] ,x["y_13"] ,x["y_14"] ,x["y_15"] ,x["y_16"] ,x["y_17"] ,x["y_18"] ,x["y_19"] ,x["y_20"] ,x["y_21"] )), lonlat=F, allpairs =T))  
    
    
    train_matrix <-list()
    for(i in 1:nrow(training_froot_loop)){
      train_matrix[[i]] <-list()
      train_matrix[[i]] <- matrix(training_dist_matrix[,i],21,21)
      }
    
 

    function_graph_list <-list(ids = data.frame(id_loop), loo_rusher = loo_rusher, y = YardList, train_x = training_features_list, train_a = train_matrix, loo_a = loo_matrix, loo_x = loo_feature_list)
  
  return(function_graph_list)
} 


all_data <-graph_processing_function(dataframe, leave_one_out = 1)

#get test train split
test <-list( y = all_data$y[test_rows], train_x = all_data$train_x[test_rows], train_a = all_data$train_a[test_rows], loo_a = all_data$loo_a[test_rows], loo_x = all_data$loo_x[test_rows])
train <-list( y = all_data$y[train_rows], train_x = all_data$train_x[train_rows], train_a = all_data$train_a[train_rows], loo_a = all_data$loo_a[train_rows], loo_x = all_data$loo_x[train_rows])
validate <-list(y = all_data$y[val_rows], train_x = all_data$train_x[val_rows], train_a = all_data$train_a[val_rows], loo_a = all_data$loo_a[val_rows], loo_x = all_data$loo_x[val_rows])


#run python code
library(reticulate)
source_python("GNN_Functions.py")


all_predictions <- py$model$predict(loader_all$load(), steps = loader_all$steps_per_epoch)
all_loo_predictions <- py$model$predict(loo_loader$load(), steps = loo_loader$steps_per_epoch)
test_predictions <- py$model$predict(loader_te$load(), steps = loader_te$steps_per_epoch)

#assess test predictions
test_eval <- data.frame(matrix(ncol = 0, nrow = 0))
for(i in 1:nrow(test_predictions)){
  predict <- test_predictions[i,]
  actual <- rep(0,120)                 
  actual[test$y[i]+19:length(actual)] <- 1
  test_eval <- rbind(test_eval, c(predict,actual))
}
names(test_eval) <- c(predict,actual)


#evaluate all predictions
#first add combandframe to dataframe to add on rushers
all_eval <- data.frame(matrix(ncol = 0, nrow = 0))
dataframe <- dataframe %>% filter(nflId == returnerId) %>% 
  unite("comb_and_frame", c(comb_id,frameId), sep= " ",remove = FALSE)
dataframe$comb_and_frame <- as.factor(dataframe$comb_and_frame)
for(i in 1:nrow(all_predictions)){
  comb_and_frame <- levels(all_data$ids$comb_and_frame)[i]
  returner <- dataframe[dataframe$comb_and_frame == comb_and_frame,"displayName"]
  predict <- min(which(all_predictions[i,] > 0.5)) -19
  actual <-all_data$y[i]
  difference <- actual - predict
  all_eval <- rbind(all_eval, c(comb_and_frame, returner, predict,actual, difference))
}
colnames(all_eval) <- c("comb_and_frame", "returner", "predict","actual", "difference")

#eval loo
loo_eval <- data.frame(matrix(ncol = 0, nrow = 0))
for(i in 1:nrow(all_loo_predictions)){
  #all_players predictions from above
  all_player_preds<-all_eval[all_eval[1] == all_data$loo_rusher[i,1]]
  #loo predictions from python
  predict <- min(which(all_loo_predictions[i,] > 0.5)) -19
  #actual values from loop
  actual <-all_data$loo_rusher[i,]
  #regex splitting out gameId to identify season
  game_id <- str_split(str_extract(all_data$loo_rusher[i,1], "[0-9]+")," ")[[1]]
  #match season to gameId
  season_id <- game_and_season[game_and_season$gameId == game_id,]
  #match player name to nflId
  player_name <- players[players$nflId == actual[,2],7]
  difference <- predict - as.numeric(actual[,3])
  loo_eval <- rbind(loo_eval, c(actual[,1],season_id[,2], actual[,2],player_name, as.numeric(actual[,3]),predict, difference))
}
names(loo_eval) <- c("combandframe",	"season",	"nflId",	"Name",	"predict",	"loo_predict", "Difference")

