#test code for joining

WorldBankData = as.data.frame(cbind(row.names(FinalWorldBankX), FinalWorldBankX))
colTemp = colnames(WorldBankData)
colTemp[1] = "key"
colnames(WorldBankData) = colTemp

#set up emdat data as data table
DisasterData = data.frame(DisasterX[,1:4])
colnames(DisasterData) = c("key", "type", "killed", "cost")

#Get final merged dataset
#Test = merge(WorldBankData, DisasterData, by="key")

X = merge(DisasterData, WorldBankData, by="key")

disasterType=unique(X[,2])

#X$type[X$type == "Mass movement dry"] <- "Mass Movement Dry"
#X$type[X$type == "Mass movement wet"] <- "Mass Movement Wet"

X$type[X$type == "Transport accident"] <- "Transport Accident"

Transport = X[X$type=="Transport Accident",]
Storm = X[X$type=="Storm",]
Earthquake = X[X$type=="Earthquake (seismic activity)",]
Flood  = X[X$type=="Flood",]
Epidemic = X[X$type=="Epidemic",]
