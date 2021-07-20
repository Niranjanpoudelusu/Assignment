##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 4


### Assignment part 1 #####

## Loading data into the environment 
load("C:/Users/Niranjan.p/Downloads/sldutah.RData")



###Extracting only the required variables into a new dataset
data <- sldutah[,c(56,78,88,100,101)]

##Making on of the variable as a categorical variable
data$D1B <- cut(data$D1B, breaks=c(0,1.854,6.011,9.453,60),labels=c("low","medium","high","veryhigh"))

###Running the linear regression model
regmod <- lm(D5ar ~ D1B + D2A_EPHHM + D3amm + D4d, data = data); summary(regmod)

###Assignment part 2 ###

###Loading the required datasets into the environment 
load("C:/Users/Niranjan.p/Downloads/atr0620.Rdata")
load("C:/Users/Niranjan.p/Downloads/atr0363.Rdata")
load("C:/Users/Niranjan.p/Downloads/loganweath.Rdata")

###Dataset of site SR 30 i.e. atr0620 will be analyzed
###First lets look what kind of data we have 
str(atr0620)
str(loganweath)

###Aggregating the dataset 
temp <- aggregate(Count ~ Date, data = atr0620 ,FUN = sum, na.rm=T)
names(temp) <- c("Date", "Total")

###Now lets add other variables also in this new dataset

temp$Year <- as.integer(strftime(temp$Date, format="%Y"))
temp$Month <- as.integer(strftime(temp$Date, format="%m"))
temp$Month <- factor(temp$Month, labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
temp$Weekday <- strftime(temp$Date, format="%a")
temp$Weekday <- factor(temp$Weekday, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

###Making a new permanent dataset
Atr <- temp[,c("Date", "Total", "Year", "Month", "Weekday")]
rm(temp)

###Now lets merge this data with the dataframe of weather
mergedata <- merge(Atr, loganweath,by.x= "Date", by.y = "DATE", all.x = T )

## Creating a time series of total count variable
mts <- ts(mergedata$Total,frequency = 7)

###Plotting the time series
par(mar=c(10,10,10,6)+0.3)
par(cex.axis = 0.7, cex.lab=1)
plot(mts, ylim=c(3000, 16000),las=1, xlab = "Week",ylab="  " ,main = "Time series of daily motor counts")
title(ylab = "Daily Count",line = 4.5)


##Decomposing my time series models
mts_decom <- stl(mts, s.window = "periodic")
plot(mts_decom, main = " Decomposition of time series of daily motor counts")

###estimating the time series regression model 
ts_reg <- lm(Total ~ Weekday + Month + TMIN + TMAX, data= mergedata)
summary(ts_reg)

par(mar=c(5,4,4,2))

###plotting the total count with respect to minimum and meximum temperature.
plot(mergedata$Total,mergedata$TMIN)
plot(mergedata$Total, mergedata$TMAX)

##The end###
