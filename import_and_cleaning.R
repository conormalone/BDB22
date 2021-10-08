library(tidyverse)
#import files needed now and set factors
plays <- read.csv("plays.csv")
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
all_tracking$gameId <- as.factor(all_tracking$gameId)
all_tracking$playId <- as.factor(all_tracking$playId)

#add combined Id to give each play a unique reference
plays$comb_id <- paste0(as.character(plays$gameId), " ", as.character(plays$playId))
all_tracking$comb_id <- paste0(as.character(all_tracking$gameId), " ", as.character(all_tracking$playId))

#subset just kick returns, kickoff and punt, not onside kicks
return_plays <- plays %>% filter(specialTeamsResult == "Return") %>% drop_na(returnerId)
rm(plays)
#subset just tracking relating to these return plays  
return_tracking <- all_tracking[all_tracking$comb_id %in% return_plays$comb_id,]
rm(all_tracking)

#add returner to tracking
return_tracking <-merge(x = return_tracking, y = return_plays[ , c("comb_id", "returnerId")], by = "comb_id", all.x=TRUE)


