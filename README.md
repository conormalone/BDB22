# BDB22
NFL Big Data Bowl 2022(Special Teams)
predicting return distance using graph network (nodes for returner, return team and kicking team)
then CV graphs to ascertain yards added by blockers or kickers

v2 with secure SSH

Done:
(8/10) import
(8/10) subset to just return plays


To Do:
(8/10) select relevant frames from return plays
(8/10) get distance to all other players "for(i in 1:nrow(IPWorking)){
  linesstore[i]<-pointDistance(c(IPWorking$X_std.x[i], IPWorking$Y_std.x[i]), c(IPWorking$X_std.y[i], IPWorking$Y_std.y[i]),lonlat=F )
}" FROM FUMBLESWORKING
(8/10) set up graph
(8/10) GNN Model
(8/10) predit Leave-One-Out blockers
