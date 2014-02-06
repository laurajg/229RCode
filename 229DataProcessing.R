agriculture = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Agriculture.csv")
aid = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Aid.csv")
climate = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Climate.csv")
economic = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Economic.csv")
education = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Education.csv")
energy = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Energy.csv")
environment = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Environment.csv")
financial = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Financial.csv")
gender = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Gender.csv")
health = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Health.csv")
infrastructure = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Infrastructure.csv")
labor = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Labor.csv")
poverty = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Poverty.csv")
private = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Private.csv")
public = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Public.csv")
science = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Science.csv")
social = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Social.csv")
urban = read.csv("/Users/laurajadegriffiths/Desktop/CSVfiles/Urban.csv")

countries= as.data.frame(unique(aid[,1]))
years= as.data.frame(substr(colnames(aid), 2, 5))
years = as.data.frame(years[5:57,])
newN = nrow(countries)*nrow(years)

#create new row names (combine countries w/ all years)
newRows = {}
for (i in 1:nrow(countries)) {
  for(j in 1:nrow(years)) {
    newRows = rbind(newRows, paste(countries[i,1], sep=" ", years[j,1]))
  }   
}

#get list of indicators for each topic
aidIndicators = as.data.frame(unique(aid[,3]))
agricultureIndicators = as.data.frame(unique(agriculture[,3]))
climateIndicators = as.data.frame(unique(climate[,3]))
economicIndicators = as.data.frame(unique(economic[,3]))
educationIndicators = as.data.frame(unique(education[,3]))
energyIndicators = as.data.frame(unique(energy[,3]))
environmentIndicators = as.data.frame(unique(environment[,3]))
financialIndicators = as.data.frame(unique(financial[,3]))
genderIndicators = as.data.frame(unique(gender[,3]))
healthIndicators = as.data.frame(unique(health[,3]))
infrastructureIndicators = as.data.frame(unique(infrastructure[,3]))
laborIndicators = as.data.frame(unique(labor[,3]))
povertyIndicators = as.data.frame(unique(poverty[,3]))
privateIndicators = as.data.frame(unique(private[,3]))
publicIndicators = as.data.frame(unique(public[,3]))
scienceIndicators = as.data.frame(unique(science[,3]))
socialIndicators = as.data.frame(unique(social[,3]))
urbanIndicators = as.data.frame(unique(urban[,3]))

#read data in as a design matrix for each topic
Xaid= as.data.frame(aid[,5:57])
Xagriculture = as.data.frame(agriculture[,5:57])
Xclimate = as.data.frame(climate[,5:57])
Xeconomic = as.data.frame(economic[,5:57])
Xeducation = as.data.frame(education[,5:57])
Xenergy = as.data.frame(energy[,5:57])
Xenvironment = as.data.frame(environment[,5:57])
Xfinancial = as.data.frame(financial[,5:57])
Xgender = as.data.frame(gender[,5:57])
Xhealth = as.data.frame(health[,5:57])
Xinfrastructure = as.data.frame(infrastructure[,5:57])
Xlabor = as.data.frame(labor[,5:57])
Xpoverty = as.data.frame(poverty[,5:57])
Xprivate = as.data.frame(private[,5:57])
Xpublic= as.data.frame(public[,5:57])
Xscience = as.data.frame(science[,5:57])
Xsocial = as.data.frame(social[,5:57])
Xurban= as.data.frame(urban[,5:57])

#create new matrices
XaidNew = matrix(0, nrow(newRows), nrow(aidIndicators))
XagricultureNew = matrix(0, nrow(newRows), nrow(agricultureIndicators))
XclimateNew = matrix(0, nrow(newRows), nrow(climateIndicators))
XeconomicNew = matrix(0, nrow(newRows), nrow(economicIndicators))
XenergyNew = matrix(0, nrow(newRows), nrow(energyIndicators))
XeducationNew = matrix(0, nrow(newRows), nrow(educationIndicators))
XenvironmentNew = matrix(0, nrow(newRows), nrow(environmentIndicators))
XfinancialNew = matrix(0, nrow(newRows), nrow(financialIndicators))
XgenderNew = matrix(0, nrow(newRows), nrow(genderIndicators))
XhealthNew = matrix(0, nrow(newRows), nrow(healthIndicators))
XinfrastructureNew = matrix(0, nrow(newRows), nrow(infrastructureIndicators))
XlaborNew = matrix(0, nrow(newRows), nrow(laborIndicators))
XpovertyNew = matrix(0, nrow(newRows), nrow(povertyIndicators))
XprivateNew = matrix(0, nrow(newRows), nrow(privateIndicators))
XpublicNew = matrix(0, nrow(newRows), nrow(publicIndicators))
XscienceNew = matrix(0, nrow(newRows), nrow(scienceIndicators))
XsocialNew = matrix(0, nrow(newRows), nrow(socialIndicators))
XurbanNew = matrix(0, nrow(newRows), nrow(urbanIndicators))

#for each country fill in matrix
for (i in 1:nrow(countries)) {
   
    XaidNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xaid[(((i-1)*nrow(aidIndicators))+1):(i*nrow(aidIndicators)),])
    XagricultureNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xagriculture[(((i-1)*nrow(agricultureIndicators))+1):(i*nrow(agricultureIndicators)),])
    XclimateNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xclimate[(((i-1)*nrow(climateIndicators))+1):(i*nrow(climateIndicators)),])
    XeconomicNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xeconomic[(((i-1)*nrow(economicIndicators))+1):(i*nrow(economicIndicators)),])
    XeducationNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xeducation[(((i-1)*nrow(educationIndicators))+1):(i*nrow(educationIndicators)),])
    XenergyNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xenergy[(((i-1)*nrow(energyIndicators))+1):(i*nrow(energyIndicators)),])
    XenvironmentNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xenvironment[(((i-1)*nrow(environmentIndicators))+1):(i*nrow(environmentIndicators)),])
    XfinancialNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xfinancial[(((i-1)*nrow(financialIndicators))+1):(i*nrow(financialIndicators)),])
    XgenderNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xgender[(((i-1)*nrow(genderIndicators))+1):(i*nrow(genderIndicators)),])
    XhealthNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xhealth[(((i-1)*nrow(healthIndicators))+1):(i*nrow(healthIndicators)),])
    XinfrastructureNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xinfrastructure[(((i-1)*nrow(infrastructureIndicators))+1):(i*nrow(infrastructureIndicators)),])
    XlaborNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xlabor[(((i-1)*nrow(laborIndicators))+1):(i*nrow(laborIndicators)),])
    XpovertyNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xpoverty[(((i-1)*nrow(povertyIndicators))+1):(i*nrow(povertyIndicators)),])
    XprivateNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xprivate[(((i-1)*nrow(privateIndicators))+1):(i*nrow(privateIndicators)),])
    XpublicNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xpublic[(((i-1)*nrow(publicIndicators))+1):(i*nrow(publicIndicators)),])
    XscienceNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xscience[(((i-1)*nrow(scienceIndicators))+1):(i*nrow(scienceIndicators)),])
    XsocialNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xsocial[(((i-1)*nrow(socialIndicators))+1):(i*nrow(socialIndicators)),])
    XurbanNew[(((i-1)*nrow(years))+1):(i*nrow(years)),] = t(Xurban[(((i-1)*nrow(urbanIndicators))+1):(i*nrow(urbanIndicators)),]) 
}


rownames(XaidNew) = newRows[,1]
rownames(XagricultureNew) = newRows[,1]
rownames(XclimateNew) = newRows[,1]
rownames(XeconomicNew) = newRows[,1]
rownames(XeducationNew) = newRows[,1]
rownames(XenergyNew) = newRows[,1]
rownames(XenvironmentNew) = newRows[,1]
rownames(XfinancialNew) = newRows[,1]
rownames(XgenderNew) = newRows[,1]
rownames(XhealthNew) = newRows[,1]
rownames(XinfrastructureNew) = newRows[,1]
rownames(XlaborNew) = newRows[,1]
rownames(XpovertyNew) = newRows[,1]
rownames(XprivateNew) = newRows[,1]
rownames(XpublicNew) = newRows[,1]
rownames(XscienceNew) = newRows[,1]
rownames(XsocialNew) = newRows[,1]
rownames(XurbanNew) = newRows[,1]

colnames(XaidNew) = aidIndicators[,1]
colnames(XagricultureNew) = agricultureIndicators[,1]
colnames(XclimateNew) = climateIndicators[,1]
colnames(XeconomicNew) = economicIndicators[,1]
colnames(XeducationNew) = educationIndicators[,1]
colnames(XenergyNew) = energyIndicators[,1]
colnames(XenvironmentNew) = environmentIndicators[,1]
colnames(XfinancialNew) = financialIndicators[,1]
colnames(XgenderNew) = genderIndicators[,1]
colnames(XhealthNew) = healthIndicators[,1]
colnames(XinfrastructureNew) = infrastructureIndicators[,1]
colnames(XlaborNew) = laborIndicators[,1]
colnames(XpovertyNew) = povertyIndicators[,1]
colnames(XprivateNew) = privateIndicators[,1]
colnames(XpublicNew) = publicIndicators[,1]
colnames(XscienceNew) = scienceIndicators[,1]
colnames(XsocialNew) = socialIndicators[,1]
colnames(XurbanNew) = urbanIndicators[,1]


#Bind together across indicator categories
WorldBankX = cbind(XaidNew, XagricultureNew, XclimateNew, XeconomicNew, XeducationNew, XeducationNew,
          XenergyNew, XenvironmentNew, XfinancialNew, XgenderNew, XhealthNew, XinfrastructureNew,
          XlaborNew, XpovertyNew, XprivateNew, XpublicNew, XscienceNew, XsocialNew, XurbanNew)

#Get rid of rows where every value is NA
WorldBankX = WorldBankX[rowSums(is.na(WorldBankX))<ncol(WorldBankX), ]

#SET MISSING VALUE THRESHOLD (i.e. max percent of data that can be missing is..)
missingThreshold = .2

#Get rid of columns w/ % missing value above threshold
Missing = {}
for (i in 1:ncol(WorldBankX)) {
  Missing = cbind(Missing,sum(is.na(WorldBankX[,i]))/nrow(WorldBankX))
}
finalIndex=which(Missing < missingThreshold)
WorldBankX=WorldBankX[,finalIndex]
FinalWorldBankX = WorldBankX[rowSums(is.na(WorldBankX))<ncol(WorldBankX), ]

#test potential imputation stuff

