##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 5

###Asingmnet part1 

#### Loading sldutah dataset into environment
load("C:/Users/Niranjan.p/Downloads/sldutah.RData")

##Loading the data required for linear regression
data <- sldutah[,c(56,78,88,100,101)]

##regression of the dependent varaible with independent variables
modreg <- lm(D5ar ~ D1B + D2A_EPHHM + D3amm + D4d, data = data); summary(modreg)

### Lets check the leverage numerically and visually
modreg_hat <- hatvalues(modreg)
head(sort(modreg_hat, decreasing=T), n=10)

##ploting the leverage values
par(mar=c(5,4,4,2))
plot(modreg_hat, pch=20, las=1,
     xlab="Observations", ylab="Levergae(h_ii", main="Leverage of observations")

##lets look at the discrepancy using studentized residuals numerically as well as visually
modreg_dis <- rstudent(modreg)
head(sort(abs(modreg_dis),decreasing=T),n=10)

#Visual inspection
plot(modreg_dis,pch=20, las=1,xlab="Observations",ylab="Studentized residuals",
     main="Studentized residuals of observations")

### checking for the influence using cooks distance
modreg_cd <- cooks.distance(modreg)
head(sort(modreg_cd,decreasing=T),n=10)

##visual check
plot(modreg_cd, pch=20, las=1, xlab="Observations", ylab="Cook's Distance",
     main="Cook's distance of observations")

##lets check all of these outliers in a single plot
##Calling the package required fot to run the function
library("car")

modreg_outl <- influencePlot(modreg, main="Outlier detection plot")

###lets identify the potential outliers
modreg$model[row.names(modreg_outl),]

###removing these potential outliers
data2 <- data[!(row.names(data) %in% row.names(modreg_outl)),]

modreg2 <- update(modreg, data = data2);summary(modreg2);summary(modreg)



##scatterplot of residuals against observations for the new updated regression model

for(i in 1 :ncol(modreg2$model)) {
  plot(modreg2$model[,i],modreg2$residuals,pch=20,xlab=names(modreg2$model)[i],ylab="residuals",
       main="Scatterplot of residuals")
  lines(x=c(min(modreg2$model[,i]),max(modreg2$model[,i])), y=c(0,0))
};rm(i)

###qq plot
qqnorm(modreg2$residuals,pch=20)
qqline(modreg2$residuals)

#mean of residuals
mean(modreg2$residuals)

#correlation among independent variables
cor(modreg2$model[2:ncol(modreg$model)])

##variance inflation factor
vif(modreg2)

##calling library for Breusch-Pagan test
library("lmtest")

#bptest
bptest(modreg2)

#Kolmogorov-Smirnov tests
ks.test(scale(modreg2$residuals, center=T, scale=T), "pnorm")


###there seems to be some NA in data as well as some zero value somewhere due to which the log transformation is not working
##remove Na and add 1 to D5ar column, because log of 0 is infinite and is causing problem with the data or we can remove the 
###observation with zero in the independent variable


data2 <- na.omit(data2)
data2$D5ar <- data2$D5ar + 1

###Third linear regression model
modreg3 <- lm(log(D5ar) ~ D1B + D2A_EPHHM +D3amm + D4d, data = data2); summary(modreg3)

###Assignment part 2
###Loading the required datasets into the environment 
load("C:/Users/Niranjan.p/Downloads/atr0620.Rdata")
load("C:/Users/Niranjan.p/Downloads/atr0363.Rdata")
load("C:/Users/Niranjan.p/Downloads/loganweath.Rdata")

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

ts_reg <- lm(Total ~ Weekday + Month + TMIN + TMAX, data= mergedata)
summary(ts_reg)

##Autocorrelation and stationary test
acf(mergedata$Total)
acf(ts_reg$residuals)

#bgtest
bgtest(ts_reg,order=7)


##### the end

