##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 2

### Loading the data into the enviironment

load("C:/Users/Niranjan.p/Downloads/atr0363.Rdata")
load("C:/Users/Niranjan.p/Downloads/atr0620.Rdata")

###Agrregate the dataframe to get daily totals

##For the first dataframe atr0620
temp <- aggregate(Count ~ Date, data = atr0620, FUN=sum , na.rm=T)
names(temp) <- c("Date", "Total")

# Now adding all other colums in temp dataframe 
temp$Year <- as.integer(strftime(temp$Date, format="%Y"))
temp$Month <- as.integer(strftime(temp$Date, format="%m"))
temp$Month <- factor(temp$Month, labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
temp$Weekday <- strftime(temp$Date, format="%a")
temp$Weekday <- factor(temp$Weekday, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

At0620 <- temp[,c("Date", "Total", "Year", "Month", "Weekday")]

##Again for the second dataset atr0363
temp <- aggregate(Count ~ Date, data = atr0363, FUN=sum , na.rm=T)
names(temp) <- c("Date", "Total")

# Now adding all other colums in temp dataframe 
temp$Year <- as.integer(strftime(temp$Date, format="%Y"))
temp$Month <- as.integer(strftime(temp$Date, format="%m"))
temp$Month <- factor(temp$Month, labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
temp$Weekday <- strftime(temp$Date, format="%a")
temp$Weekday <- factor(temp$Weekday, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

At0363 <- temp[,c("Date", "Total", "Year", "Month", "Weekday")]

summary(atr0620)

##data frame with monthly avergae counts for atr0620 (SR30)
temp <- aggregate(Total ~ Month, data = At0620, FUN = mean, na.rm=T)


##data frame with monthly average counts for atr0363 (SR91)
temp1 <- aggregate(Total ~ Month, data = At0363, FUN = mean, na.rm=T)

##barplot from these temporary data for each datasets
barplot(temp$Total,names.arg= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),las=2,
        main = "Barplot of average daily motor vehicles counts by month for site SR30 ",
        xlab = "Months", ylab= "Average daily moto vehicles by month", ylim = c(0,14000), xlim = c(0,18), space = 0.5, cex.axis =0.8 )

barplot(temp1$Total,names.arg= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),las=2,
        main = "Barplot of average daily motor vehicles counts by month for site SR91 ",
        xlab = "Months", ylab= "Average daily moto vehicles by month", ylim = c(0,30000),xlim = c(0,18), space = 0.5, cex.axis =0.7,
        cex.names = 0.9)

###removing these tempoarary dataframes which are no needed
rm(temp)
rm(temp1)

##subseting the data frames for two months january and august
##sub0363<- subset(At0363,At0363$Month=="Jan" |At0363$Month=="Aug")

sub0620jan<- subset(At0620,At0620$Month=="Jan")
sub0620aug<- subset(At0620,At0620$Month=="Aug")


sub0363jan<- subset(At0363,At0363$Month=="Jan")
sub0363aug<- subset(At0363,At0363$Month=="Aug")

## Calculation of mean for both sites and both time periods
##mean and variance for january and august  for SR30

mean(sub0620jan$Total)
mean(sub0620aug$Total)
var(sub0620jan$Total)
var(sub0620aug$Total)

##mean and variance of january and august for SR91
mean(sub0363jan$Total)
mean(sub0363aug$Total)
var(sub0363jan$Total)
var(sub0363aug$Total)

###let us try some hypothesis for the month of august for both the sites
## for site SR30 null hypothesis: mean = 11000 and alternate hypothesis: mean is not equal to 11000

Sr30 <- t.test(sub0620aug$Total, mu = 11000); Sr30

## for site SR91 null hypothesis: mean = 20000 and alternate hypothesis: mean is not equal to 20000
Sr91 <- t.test(sub0363aug$Total, mu = 20000); Sr91

### Hypothesis test for the equivalency of variances of daily traffic for two months for site SR30
### Null hypothesis: Variance of traffic in august = Variance of traffic in january

VarSR30 <- var.test(sub0620jan$Total,sub0620aug$Total, ratio=1); VarSR30

### Hypothesis test for the equivalency of mean for two months for site SR30
###Null hypothesis: mean of traffic counts in august = mean of traffic count in jauary
Mean_jan_aug <- t.test(sub0620jan$Total,sub0620aug$Total, mu=0, var.equal = T); Mean_jan_aug


##paired samples hypothesis test about means

###paired t-test for the equivalency of mean for the month of august for both sites


## Null hypothesis for pared sampled test is Mean daily traffic in august for SR30 = mean daily traffic in august for SR91
## Alternate hypothesis those mean are not equal

t_paired <- t.test(sub0363aug$Total,sub0620aug$Total, mu =0, paired= T); t_paired


 #####the end




