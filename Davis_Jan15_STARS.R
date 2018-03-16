library(readr)
library(tidyr)
library(ggplot2)
library(stats)
library(Hmisc)
library(reshape2)

options(scipen=999)

# LOAD THE DATA (this is SC and AGR data, will only use AGR data)
library(readxl)
Combo_Nov_26_17 <- read_excel("C:/Users/Alex/Dropbox/Davis_Thesis_Sustainability and Alumni Giving/Data Analysis/Combo_Dec_03_17.xlsx", 
na = "NA")
# Combo_Nov_26_17 <- read_excel("/Users/nathankarst/Dropbox/Student Research/Davis_Thesis_Sustainability and Alumni Giving/Data Analysis/Combo_Dec_03_17.xlsx", na = "NA")

# LOAD THE STARS DATA
library(readxl)
STARS <- read_excel("C:/Users/Alex/Dropbox/Davis_Thesis_Sustainability and Alumni Giving/stars/stars_filled_wide.xlsx", 
na = "NA")
# STARS <- read_excel("/Users/nathankarst/Dropbox/Student Research/Davis_Thesis_Sustainability and Alumni Giving/stars/stars_filled_wide.xlsx", na = "NA")

## Clean up the Alumni Giving Rate data:
# Make a data table, called AGR, of Alumni Giving Data by slicing out the columns for IPEDS ID and AGR for each year
AGR = Combo_Nov_26_17 [,c(1,14:24)]
# in new version, this is still old NA-ful data


# Create AGR.Count, which adds a column with the count of the number of years of data for each institution to AGR. 
AGR.Count <- rowSums(!is.na(AGR[,2:12]))
AGR.Count = data.frame ( AGR, AGR.Count)
# in new version, this should be data.frame(NEW AGR data, AGR.Count)

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


##	Clean up the STARS Data:
# Create data table called STARS.Count that notes the number of data points (annual scores) for each institution
# Starting at column 3 means we don't count 2010, unless it was forward filled (only three schools have scores from 2010)
STARS.Count <- rowSums(!is.na(STARS[,3:9]))
STARS.Count = data.frame ( STARS, STARS.Count)

# Create a table called STARS.Keep, which remove institutions with less than 7 out of 7 years of data (simple count).
# Data set has data from 2011-2017. 
STARS.Keep <- subset(STARS.Count, STARS.Count > 6) 

# Change the column headers for STARS rankings data to simple numbers (but text) in preparation for making them numeric
colnames(STARS.Keep) = c("IPEDS.ID","2010","2011","2012","2013","2014","2015","2016","2017", "STARS_Count")

# Create a long form data version of the STARS data. Extra data needed to be sliced prior to avoid error.
STARS.LongPrep = STARS.Keep[,c(1,3:9)]
STARS.Long = gather(STARS.LongPrep, "Year", "Score", 2:8)

# Classifies the Year data as a number
STARS.Long$Year = as.numeric(STARS.Long$Year)


#######################################################################################
#######################################################################################


## Join the STARS scores and Alumni Giving Data into a new four column long form data table,
## called Combo.Long, with columns of: IPEDS.ID, Year, AGR, STARS Score

# merge(df1, df2, by ="ID")
Combo.Long <- merge(STARS.Long, AGR.SimpleLong, by=c("IPEDS.ID","Year"))


#######################################################################################
#######################################################################################


## Spearman Correlation of SIERRA data over time
colnames(STARS) = c("IPEDS.ID","2010","2011","2012","2013","2014","2015","2016","2017")
cormatrix = rcorr(as.matrix(STARS[,c(3:9)]), type='spearman')
cordata = melt(cormatrix$r)
colnames(cordata)[3] = "Spearman"
ggplot(cordata, aes(x=Var1, y=Var2, fill=`Spearman`)) + geom_tile() + xlab("") + ylab("") + scale_x_continuous(breaks=c(2011:2017)) + scale_y_continuous(breaks=c(2011:2017))

## Shows STARS data does not jump around the way Sierra does

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
# Requires wide form data set with institutions by row and columns of STARS rankings and Alumni giving rates
# Uses infilled data sets joined by IPEDS.ID


# merge(df1, df2, by ="ID")
Combo.Wide <- merge(STARS.LongPrep, AGR.Simple, by=c("IPEDS.ID"))

## LAGGING LINEAR MODELS
# STARS score column headers are just years
m13 = lm(`AG.FY13` ~ `2011` + `2012` + `2013` + `AG.FY11` + `AG.FY12`, data = Combo.Wide)
m14 = lm(`AG.FY14` ~ `2012` + `2013` + `2014` + `AG.FY12` + `AG.FY13`, data = Combo.Wide)
m15 = lm(`AG.FY15` ~ `2013` + `2014` + `2015` + `AG.FY13` + `AG.FY14`, data = Combo.Wide)
m16 = lm(`AG.FY16` ~ `2014` + `2015` + `2016` + `AG.FY14` + `AG.FY15`, data = Combo.Wide)
m17 = lm(`AG.FY17` ~ `2015` + `2016` + `2017` + `AG.FY15` + `AG.FY16`, data = Combo.Wide)

## LAGGING LINEAR MODELS FOR EACH SCHOOL
# I was having issues with the STARS_lag2 part, so I just hashed it out. 
schools = Combo.Wide$IPEDS.ID
i = 1

AG_lag1_coeff = rep(NA,length(schools))
AG_lag2_coeff = rep(NA,length(schools))
STARS_lag1_coeff = rep(NA,length(schools))
STARS_lag2_coeff = rep(NA,length(schools))

AG_lag1_pval = rep(NA,length(schools))
AG_lag2_pval = rep(NA,length(schools))
STARS_lag1_pval = rep(NA,length(schools))
STARS_lag2_pval = rep(NA,length(schools))

# runs fine with the above 2 STARS_lag2_X parts

for (school in schools){
  print(school)
  temp = Combo.Wide[Combo.Wide$IPEDS.ID == school,]
  
  target = unlist(temp[1,c(15:19)])
  ag_lag1 = unlist(temp[1,c(14:18)])
  ag_lag2 = unlist(temp[1,c(13:17)])
  STARS_lag1 = unlist(temp[1,c(3:7)])
  STARS_lag2 = unlist(temp[1,c(2:6)]) 
  
  lagging = data.frame(target, ag_lag1, ag_lag2, STARS_lag1, STARS_lag2)
  #lagging = data.frame(target, ag_lag1, ag_lag2, STARS_lag1,)
  if (nrow(na.omit(lagging)) > 4) {
    model = lm(target ~ ., data=lagging)
    coeffs = summary(model)$coefficients
    
    
    AG_lag1_coeff[i] = coeffs[2,1]
    AG_lag2_coeff[i] = coeffs[3,1]
    STARS_lag1_coeff[i] = coeffs[4,1]
    STARS_lag2_coeff[i] = coeffs[5,1]
    
    AG_lag1_pval[i] = coeffs[2,4]
    AG_lag2_pval[i] = coeffs[3,4]
    STARS_lag1_pval[i] = coeffs[4,4]
    STARS_lag2_pval[i] = coeffs[5,4]    
    print(school)
    print(coeffs) 
    print(lagging)    
    
  }
  
  
  i = i + 1
  
}

#lagging_model = data.frame(schools,AG_lag1_coeff,AG_lag2_coeff,STARS_lag1_coeff,STARS_lag2_coeff,AG_lag1_pval,AG_lag2_pval,STARS_lag1_pval,STARS_lag2_pval)
lagging_model = data.frame(schools,AG_lag1_coeff,AG_lag2_coeff,STARS_lag1_coeff,AG_lag1_pval,AG_lag2_pval,STARS_lag1_pval)


##########################
##########################

## LINEAR CHANGE IN GIVING
AGR.Slope <- data.frame(AGR.Infill$IPEDS.ID,AGR.Slope)
colnames(AGR.Slope) = c("IPEDS.ID","AGR.Slope")

## LINEAR CHANGE IN Rankings
schools = STARS.LongPrep$IPEDS.ID
#Makes an empty vector with a length equal to the number of schools
STARS.Slope = rep(0, length(schools))
i = 1
for (school in schools){
  tmp = STARS.Long[STARS.Long$IPEDS.ID == school,]
  if (nrow(tmp) < 4){
    STARS.Slope[i] = NA
    i = i + 1
    next
  }
  
  model = lm(Score ~ Year, data=tmp)
  STARS.Slope[i] = model$coefficients[2]
  i = i+1
}
STARS.Slope <- data.frame(STARS.Keep$IPEDS.ID,STARS.Slope)
colnames(STARS.Slope) = c("IPEDS.ID","STARS.Slope")

# Merge the *.Slope lists
Dual.Slopes <- merge(STARS.Slope, AGR.Slope, by.y = "IPEDS.ID")

# Positive AGR slope and negative STARS slope
Group1 <- Dual.Slopes[Dual.Slopes$STARS.Slope >0 & Dual.Slopes$AGR.Slope > 0,]

# Negative AGR slope and negative STARS slope
Group2 <- Dual.Slopes[Dual.Slopes$STARS.Slope >0 & Dual.Slopes$AGR.Slope < 0,]