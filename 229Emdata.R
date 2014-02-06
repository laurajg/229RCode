emdataRaw = read.delim("/Users/laurajadegriffiths/Desktop/CSVfiles/emdata.tsv", row.names=NULL)
newColNames = c("Start", "End", "Country", "Location", "Type", "Sub_Type", "Name", "Killed", "Cost", "Affected", "Id")
colnames(emdataRaw)=newColNames

#include 10 if you want to include affected
Index=c(3, 5, 8, 9)

month = as.integer(substr(emdataRaw[,1], 3, 4))
year=as.integer(substr(emdataRaw[,1],5,8 ))

DisasterX = emdataRaw[,Index]
DisasterX = cbind(DisasterX, month, year)

#make sure to updata year col so is accurate if change subset
DisasterX[,1] = paste(DisasterX[,1], sep=" ", DisasterX[,6])





#checks how much data missing in a col
Missing = {}
for (i in 1:ncol(emdata)) {
  Missing = cbind(Missing,sum(is.na(emdata[,i]))/nrow(emdata))
}