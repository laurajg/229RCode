########################################################################################################
X2L = read.csv("/home/laura/Desktop/X2L.csv")       #Matrix 1: .6 col, .6 row                            #EPIDEMIC KILLED
X3L = read.csv("/home/laura/Desktop/X3L.csv")       #Matrix 1: .7 col, .5 row                            #STORM KILLED, FLOOD KILLED
X4L = read.csv("/home/laura/Desktop/X4L.csv")       #Matrix 1: .7 col, .6 row                            #QUAKE KILLED, STORM COST
X1C = read.csv("/home/laura/Desktop/imputedMatrix1R.csv")       #Matrix 1: .6 col, .5 row                #FLOOD COST
X2C = read.csv("/home/laura/Desktop/imputedMatrix2R.csv")        #Matrix 2: .6 col, .6 row               #EPIDEMIC COST
X3C = read.csv("/home/laura/Desktop/imputedMatrix3R.csv")         #Matrix 3: .7 col, .5row               #QUAKE COST
X4C = read.csv("/home/laura/Desktop/imputedMatrix4R.csv")         #Matrix 4: .7 col, .6row
########################################################################################################



############## CHANGE THIS - PICK THE DATA SET AND SETUP ANY VARIABLES FOR CLUSTERING ###################
X= X3L
numComp = 5
numWBClusters = 4
numDisasterClusters = 3
########################################################################################################



###################################### DO NO CHANGE THIS SECTION ######################################################################
### Basic Pre-Processing - Do Not Edit ###
X = X[,-1]
X = X[!is.na(X$Cost),]
X = X[!is.na(X$Killed),]
X$Cost = log(X$Cost)
X$Killed = log(X$Killed)

#### Don't worry about this ####
EpidemicX = X[X$Epidemic ==1,]
FloodX = X[X$Flood ==1,]
StormX = X[X$Storm ==1,]
QuakeX = X[X$Earthquake..seismic.activity. ==1,]
######################################################################################################################################




######################################################################################################################################
#UNCOMMENT THIS LINE AND CHANGE TO RELEVANT DISASTER IF WANT TO RUN ON JUST DATA FOR COUNTRIES WHO HAVE EXPERIENCED AT LEAST ONE OF THAT DISASTER TYPE
######################################################################################################################################

 #X= QuakeX
######################################################################################################################################




###################################### DO NOT CHANGE THIS SECTION ######################################################################

############# Get WB and Disaster Sub-Matrices for Clustering  ############
WBstart = which(colnames(X) == "Mass.Movement.Dry") +1   ## Full Matrix
Disaster = X[,2:(WBstart-1)]
WB = X[,WBstart:ncol(X)]
###########################################################################

#############    PCA on world bank features    ############################
scaledWB = scale(WB)
prinRes = princomp(scaledWB)
featuresWB = prinRes$scores[,1:numComp]   

#############      Cluster First Time     #################################
cl <- kmeans(featuresWB, numWBClusters)
clusterCol1 = as.matrix(cl$cluster)
colnames(clusterCol1) = 'ClusterNumWB'
###########################################################################

#############      Cluster Second time    #################################
c2 <- kmeans(Disaster, numDisasterClusters)
clusterCol2 = as.matrix(c2$cluster)
colnames(clusterCol2) = "ClusterNumDisaster"
###########################################################################

############## Bind Together Data Matrix and Cluster Information ##########
XTemp <- cbind(X[,1:(WBstart-1)], featuresWB, clusterCol1, clusterCol2)
###########################################################################

######################################################################################################################################





######################################################################################################################################
#IF CLUSTERED ON FULL SET, NOT DISASTER SPECIFIC RUN THIS TO GET RID OF IRRELEVANT TYPES AND  THE EMPTY TYPE COLUMNS NOW FOR PREDICTION
#OTHERWISE IF CLUSTERED ALREADY ON DISASTER SPECIFIC SET ABOVE, COMMENT THIS OUT
######################################################################################################################################
EpidemicX = XTemp[XTemp$Epidemic ==1,]
FloodX = XTemp[XTemp$Flood ==1,]
StormX = XTemp[XTemp$Storm ==1,]
QuakeX = XTemp[XTemp$Earthquake..seismic.activity. ==1,]

##################### CHANGE THIS FOR DESIRED DISASTER TYPE ########################
X= QuakeX
####################################################################################

WBstart = which(colnames(X) == "Mass.Movement.Dry") +1  
Typestart = which(colnames(X) == "Drought")
Disaster = X[,1:(Typestart-1)]
WB = X[,WBstart:ncol(X)]

XTemp = cbind(Disaster, WB)
#######################################################################################################################################




#######################################################################################################################################
#PROCESSING TO SEPARATE COUNTRY AND YEAR FOR PLOT FOR FINAL PAPER
#CAN ALSO MOVE THIS CODE UP ONE SECTION (COPY AND PASTE ALL OF THIS EXACTLY AS IS) AND WILL RUN ON THE FULL SET NOT JUST ONE DISASTER TYPE
#######################################################################################################################################
Country = matrix(0, nrow(XTemp), 1)
Year = matrix(0, nrow(XTemp), 1)

for (i in 1:nrow(XTemp)) {
  Key = XTemp[i, 1]
  Length = str_length(Key)
  Country[i] = substr(Key, 1, Length-5)
  Year[i] = substr(Key, Length-3, Length)
}

XForPlot = cbind(Country, Year, XTemp[, -1])

###### If want to get just a specific year #######
specificYear = 2008
XForPlotYear = XForPlot[XForPlot$Year == specificYear, ]


#######################################################################################################################################






#######################################################################################################################################
# GET AND OUTPUT RESULTS
#######################################################################################################################################

RESULTS = matrix(0, 9, 1)

for (k in 1:numWBClusters) {
  
  ClusterWB = XTemp[XTemp$ClusterNumWB == k,]
  disasterClusters = as.matrix(unique(ClusterWB$ClusterNumDisaster))
  numDisasterClusters = nrow(disasterClusters)
  
  for (j in 1:numDisasterClusters ) {
    
    ClusterFinal = ClusterWB[ClusterWB$ClusterNumDisaster == disasterClusters[j], ]
    ClusterFinal = ClusterFinal[, 2:(ncol(ClusterFinal)-2)]
    
    X=ClusterFinal
    
    if(nrow(ClusterFinal) > 10) {
      KilledModel<- lm(Killed~., data=X)
      CostModel<- lm(Cost~., data=X)
      
      ## All below the same ###
      cvKilled <- cv.lm(df=X, KilledModel, m=10, printit = FALSE)
      
      cvCost <- cv.lm(df=X, CostModel, m=10, printit = FALSE)
      results = matrix(0, 9, 1)
      
      results[1] = nrow(X)
      results[2] = summary(CostModel)$r.squared
      results[3] = mean(residuals(CostModel)^2)
      mseCost = mean((cvCost$cvpred - cvCost$Cost)^2)
      results[5] = mseCost
      compareCost = cbind(cvCost$cvpred, cvCost$Cost)
      results[4] = corr(compareCost)^2
      
      results[6] = summary(KilledModel)$r.squared
      results[7] = mean(residuals(KilledModel)^2)
      mseKilled = mean((cvKilled$cvpred - cvKilled$Killed)^2)
      results[8] = corr(compareKilled)^2
      results[9] = mseKilled
      compareKilled = cbind(cvKilled$cvpred, cvKilled$Killed)
      
      
      colnames(results) = paste("Size: (", nrow(X), ", ", ncol(X), ")")
      RESULTS = cbind(RESULTS, results)
    }
    
    
  }
}
(RESULTS = RESULTS[,-1])


############## GET AND OUTPUT RESULTS AVERAGED OVER ALL CLUSTERS WITH AT LEAST THE NUMBER OF MINIMUM REQUIRED ROWS ###########
minRequiredRows = 100
FinalResultsCalc = matrix(0, 8, 2)

for (i in 1:ncol(RESULTS)) {
  rows= RESULTS[1, i]
  if (rows >= minRequiredRows) {
    FinalResultsCalc[,1] = FinalResultsCalc[,1] + RESULTS[2:9,i]
    FinalResultsCalc[,2] = FinalResultsCalc[,2] + 1
  }
}

View(FinalResultsCalc[,1] / FinalResultsCalc[,2])