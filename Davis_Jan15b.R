## RStudio
## Version 1.1.383 - © 2009-2017 RStudio, Inc.

##R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

library(readr)
library(tidyr)
library(ggplot2)
library(stats)
library(Hmisc) # for rcorr
library(reshape2) # for melt

options(scipen=999)

# LOAD THE DATA
library(readxl)
Combo_Nov_26_17 <- read_excel("C:/Users/Alex/Dropbox/Davis_Thesis_Sustainability and Alumni Giving/Data Analysis/Combo_Jan_15_18.xlsx", 
na = "NA")

## Clean up the Alumni Giving Rate data:
# Make a data table, called AGR, of Alumni Giving Data by slicing out the columns for IPEDS ID and AGR for each year
AGR = Combo_Nov_26_17 [,c(1,14:24)]

# Create AGR.Count, which adds a column with the count of the number of years of data for each institution to AGR. 
AGR.Count <- rowSums(!is.na(AGR[,2:12]))
AGR.Count = data.frame ( AGR, AGR.Count)

# Create AGR.Keep, which removes institutions that don't have at least
# 6 out of 11 years of data (simple count).
AGR.Keep <- subset(AGR.Count, AGR.Count > 5) 

# Create AGR.Mean, which holds the means for each institution (excepting NAs)
# Create AGR.Infill as a combination of AGR.Keep and AGR.Mean
AGR.Mean <- rowMeans(AGR.Keep[,2:12],na.rm=TRUE)
AGR.Infill = data.frame(AGR.Keep, AGR.Mean)

# Infill the average for all the NAs with that institution's mean
AGR.Infill$AG.FY07[is.na(AGR.Infill$AG.FY07)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY07))]
AGR.Infill$AG.FY08[is.na(AGR.Infill$AG.FY08)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY08))]
AGR.Infill$AG.FY09[is.na(AGR.Infill$AG.FY09)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY09))]
AGR.Infill$AG.FY10[is.na(AGR.Infill$AG.FY10)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY10))]
AGR.Infill$AG.FY11[is.na(AGR.Infill$AG.FY11)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY11))]
AGR.Infill$AG.FY12[is.na(AGR.Infill$AG.FY12)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY12))]
AGR.Infill$AG.FY13[is.na(AGR.Infill$AG.FY13)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY13))]
AGR.Infill$AG.FY14[is.na(AGR.Infill$AG.FY14)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY14))]
AGR.Infill$AG.FY15[is.na(AGR.Infill$AG.FY15)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY15))]
AGR.Infill$AG.FY16[is.na(AGR.Infill$AG.FY16)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY16))]
AGR.Infill$AG.FY17[is.na(AGR.Infill$AG.FY17)] = AGR.Mean[which(is.na(AGR.Infill$AG.FY17))]

# Sets aside basic version of the infilled data with its text headers
AGR.Simple = AGR.Infill[,c(1,2:12)]

# Change the column headers for AGR data to simple numbers (but text) in preparation for making them numeric
# AGR.Infill is a wide form dataset that has alumni giving infilled for all institutions with a row for 
# each institution, labeled by IPEDS.ID
colnames(AGR.Infill) = c("IPEDS.ID","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017", "AGR_Count", "AGR_Mean")

# Create a long form data version of the AGR data
AGR.Long = gather(AGR.Infill, "Year", "Giving", 2:12)

# Strip Count and Mean from AGR.Long
AGR.SimpleLong = AGR.Long[,c(1,4:5)]

# Classifies the Year data as a number
AGR.SimpleLong$Year = as.numeric(AGR.SimpleLong$Year)


#######################################################################################
#######################################################################################


##	Clean up the Sierra Club Data:
# Make a data table called SC by slicing out the columns for IPEDS ID and SC for each year
SC = Combo_Nov_26_17 [,c(1,5:13)]

# Create data table called SC.Count that notes the number of data points (years of rankings) for each institution
SC.Count <- rowSums(!is.na(SC[,2:10]))
SC.Count = data.frame ( SC, SC.Count)

# Create a table called SC.Keep, which remove institutions with less than 5 out of 9 years of data (simple count).
# Data set has data from 2009-2017. 
SC.Keep <- subset(SC.Count, SC.Count > 4) 

# Calculate the median ranking for each institution (excepting NAs), in a table called SC.Median
SC.Median = apply(t(SC.Keep[,2:10]),2,median,na.rm=T)
# SC.keep$SC.Median = SC.Median
# This seems to duplicate the Median as another column so I am # it out now and will delete later.
# Creates a table called SC.Infill to hold data for infilling missing values
SC.Infill <- data.frame(SC.Keep, SC.Median)

# Infill the median ranking of each institution for all the NAs. 
SC.Infill$SC.09[is.na(SC.Infill$SC.09)] = SC.Median[which(is.na(SC.Infill$SC.09))]
SC.Infill$SC.10[is.na(SC.Infill$SC.10)] = SC.Median[which(is.na(SC.Infill$SC.10))]
SC.Infill$SC.11[is.na(SC.Infill$SC.11)] = SC.Median[which(is.na(SC.Infill$SC.11))]
SC.Infill$SC.12[is.na(SC.Infill$SC.12)] = SC.Median[which(is.na(SC.Infill$SC.12))]
SC.Infill$SC.13[is.na(SC.Infill$SC.13)] = SC.Median[which(is.na(SC.Infill$SC.13))]
SC.Infill$SC.14[is.na(SC.Infill$SC.14)] = SC.Median[which(is.na(SC.Infill$SC.14))]
SC.Infill$SC.15[is.na(SC.Infill$SC.15)] = SC.Median[which(is.na(SC.Infill$SC.15))]
SC.Infill$SC.16[is.na(SC.Infill$SC.16)] = SC.Median[which(is.na(SC.Infill$SC.16))]
SC.Infill$SC.17[is.na(SC.Infill$SC.17)] = SC.Median[which(is.na(SC.Infill$SC.17))]

# Sets aside an infilled table that retains text column headers
SC.Simple = SC.Infill[,c(1,2:10)]

# Change the column headers for SC rankings data to simple numbers (but text) in preparation for making them numeric
colnames(SC.Infill) = c("IPEDS.ID","2009","2010","2011","2012","2013","2014","2015","2016","2017")

# Create a long form data version of the SC data. Extra data needed to be sliced prior to avoid error.
SC.LongPrep = SC.Infill[,c(1,2:10)]
SC.Long = gather(SC.LongPrep, "Year", "Ranking", 2:10)

# Classifies the Year data as a number
SC.Long$Year = as.numeric(SC.Long$Year)


#######################################################################################
#######################################################################################


## Join the Sierra Club rankings and Alumni Giving Data into a new four column long form data table,
## called Combo.Long, with columns of: IPEDS.ID, Year, AGR, SC Ranking

# merge(df1, df2, by ="ID")
Combo.Long <- merge(SC.Long, AGR.SimpleLong, by=c("IPEDS.ID","Year"))


#######################################################################################
#######################################################################################


## Spearman Correlation of SIERRA data over time
colnames(SC) = c("IPEDS.ID","2009","2010","2011","2012","2013","2014","2015","2016","2017")
cormatrix = rcorr(as.matrix(SC[,c(2:10)]), type='spearman')
cordata = melt(cormatrix$r)
colnames(cordata)[3] = "Spearman"
ggplot(cordata, aes(x=Var1, y=Var2, fill=`Spearman`)) + geom_tile() + xlab("") + ylab("") + scale_x_continuous(breaks=c(2009:2017)) + scale_y_continuous(breaks=c(2009:2017))

# Shows that the data for 2009-2011 is correlated and the 2012-2017 data is correlated. 
# Break in correlation shows matches the methodology change from 2011 to 2012. 
# Lack of strong correlation within 2012-2017 block likely from introduction of divestment questions


#######################################################################################
#######################################################################################


## Normalized Alumni Giving Investigation
# Copies the AGR data frame of the alumni giving data to one for norm. analysis
AGR.Norm = AGR

# Changes the years labels to numerals
colnames(AGR.Norm) = c("IPEDS.ID","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")

#Turns all the Alumni Giving %'s into a comparison with the first year of data's number in the _norm data frame
AGR.Norm[,c(2:12)] = data.frame(t(apply(AGR[,c(2:12)], 1, function(x)(x/x[1]))))
AGR.Norm = na.omit(AGR.Norm)

# Makes long form data
AGR.NormLong = gather(AGR.Norm, "Year", "Normalized Giving", 2:12)
AGR.NormLong$Year = as.numeric(AGR.NormLong$Year)

# Highlights particular schools
temp = AGR.NormLong[which(AGR.NormLong$IPEDS.ID %in% c("243744","129215","222983")),] 
ggplot() + geom_line(data=AGR.NormLong, aes(x=Year, y=`Normalized Giving`, group=IPEDS.ID),colour="GRAY",alpha=0.3) + geom_line(data=temp, aes(x=Year, y=`Normalized Giving`, color=IPEDS.ID), size=.75)  + scale_x_continuous(breaks=c(2007:2017))

#Some sort of issue above in this code. I can't get the three schools to be highlighted

#########################
##########################

## LINEAR CHANGE IN GIVING
schools = AGR.Infill$IPEDS.ID
#Makes an empty vector with a length equal to the number of schools
AGR.Slope = rep(0, length(schools))
i = 1
for (school in schools){
  tmp = AGR.Long[AGR.Long$IPEDS.ID == school,]
  tmp$Year = as.numeric(tmp$Year)
  if (nrow(tmp) < 4){
    AGR.Slope[i] = NA
    i = i + 1
    next
  }
  
  model = lm(Giving ~ Year, data=tmp)
  AGR.Slope[i] = model$coefficients[2]
  i = i+1
}

hist(AGR.Slope,main='Average increase in alumni giving [2007 - 2017]',xlab="Percent")


##########################
##########################


## Analysis using lagging linear models
# Requires wide form data set with institutions by row and columns of Sierra rankings and Alumni giving rates
# Uses infilled data sets joined by IPEDS.ID


# merge(df1, df2, by ="ID")
Combo.Wide <- merge(SC.Simple, AGR.Simple, by=c("IPEDS.ID"))

## LAGGING LINEAR MODELS
m11 = lm(`AG.FY11` ~ `SC.09` + `SC.10` + `AG.FY09` + `AG.FY10`, data = Combo.Wide)
m12 = lm(`AG.FY12` ~ `SC.10` + `SC.11` + `AG.FY10` + `AG.FY11`, data = Combo.Wide)
m13 = lm(`AG.FY13` ~ `SC.11` + `SC.12` + `AG.FY11` + `AG.FY12`, data = Combo.Wide)
m14 = lm(`AG.FY14` ~ `SC.12` + `SC.13` + `AG.FY12` + `AG.FY13`, data = Combo.Wide)
m15 = lm(`AG.FY15` ~ `SC.13` + `SC.14` + `AG.FY13` + `AG.FY14`, data = Combo.Wide)
m16 = lm(`AG.FY16` ~ `SC.14` + `SC.15` + `AG.FY14` + `AG.FY15`, data = Combo.Wide)
m17 = lm(`AG.FY17` ~ `SC.15` + `SC.16` + `AG.FY15` + `AG.FY16`, data = Combo.Wide)

## LAGGING LINEAR MODELS FOR EACH SCHOOL
schools = Combo.Wide$IPEDS.ID
i = 1

AG_lag1_coeff = rep(NA,length(schools))
AG_lag2_coeff = rep(NA,length(schools))
SC_lag1_coeff = rep(NA,length(schools))
SC_lag2_coeff = rep(NA,length(schools))

AG_lag1_pval = rep(NA,length(schools))
AG_lag2_pval = rep(NA,length(schools))
SC_lag1_pval = rep(NA,length(schools))
SC_lag2_pval = rep(NA,length(schools))

for (school in schools){
  temp = Combo.Wide[Combo.Wide$IPEDS.ID == school,]
  
  target = unlist(temp[1,c(16:21)])
  ag_lag1 = unlist(temp[1,c(15:20)])
  ag_lag2 = unlist(temp[1,c(14:19)])
  sc_lag1 = unlist(temp[1,c(4:9)])
  sc_lag2 = unlist(temp[1,c(3:8)]) 
  
  lagging = data.frame(target, ag_lag1, ag_lag2, sc_lag1, sc_lag2)
  if (nrow(na.omit(lagging)) > 4) {
    model = lm(target ~ ., data=lagging)
    coeffs = summary(model)$coefficients
    AG_lag1_coeff[i] = coeffs[2,1]
    AG_lag2_coeff[i] = coeffs[3,1]
    SC_lag1_coeff[i] = coeffs[4,1]
    SC_lag2_coeff[i] = coeffs[5,1]
    
    AG_lag1_pval[i] = coeffs[2,4]
    AG_lag2_pval[i] = coeffs[3,4]
    SC_lag1_pval[i] = coeffs[4,4]
    SC_lag2_pval[i] = coeffs[5,4]    
  }
  
  i = i + 1
  
}

lagging_model = data.frame(schools,AG_lag1_coeff,AG_lag2_coeff,SC_lag1_coeff,SC_lag2_coeff,AG_lag1_pval,AG_lag2_pval,SC_lag1_pval,SC_lag2_pval)


##########################
##########################

## LINEAR CHANGE IN GIVING
AGR.Slope <- data.frame(AGR.Infill$IPEDS.ID,AGR.Slope)
colnames(AGR.Slope) = c("IPEDS.ID","AGR.Slope")

## LINEAR CHANGE IN Rankings
schools = SC.Infill$IPEDS.ID
#Makes an empty vector with a length equal to the number of schools
SC.Slope = rep(0, length(schools))
i = 1
for (school in schools){
  tmp = SC.Long[SC.Long$IPEDS.ID == school,]
  if (nrow(tmp) < 4){
    SC.Slope[i] = NA
    i = i + 1
    next
  }
  
  model = lm(Ranking ~ Year, data=tmp)
  SC.Slope[i] = model$coefficients[2]
  i = i+1
}
SC.Slope <- data.frame(SC.Infill$IPEDS.ID,SC.Slope)
colnames(SC.Slope) = c("IPEDS.ID","SC.Slope")

# Merge the *.Slope lists
Dual.Slopes <- merge(SC.Slope, AGR.Slope, by.y = "IPEDS.ID")

# Positive AGR slope and negative SC slope
Group1 <- Dual.Slopes[Dual.Slopes$SC.Slope >0 & Dual.Slopes$AGR.Slope > 0,]

# Negative AGR slope and negative SC slope
Group2 <- Dual.Slopes[Dual.Slopes$SC.Slope >0 & Dual.Slopes$AGR.Slope < 0,]

