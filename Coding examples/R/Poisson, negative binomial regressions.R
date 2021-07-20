##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 9

###Loading the dataset
library("sp")
library("rgdal")
load("C:/Users/Niranjan.p/Downloads/crashes_segments.RData")

###Susetting Freeway, Urban and all crsahes
summary(crashes_segments$FUNC_CLASS)
crash_seg <- subset(crashes_segments, crashes_segments$FUNC_CLASS=="Interstate" | crashes_segments$FUNC_CLASS=="Other Freeway-Expresway")
crash_seg <- subset(crash_seg, crash_seg$AREA_TYPE=="Urban")

###removing the Na in data set and checking for aadt dataset
crash_seg <- na.omit(crash_seg)
summary(crash_seg$AADT)
crash_seg$DIST[crash_seg$DIST < 0.0001] <- NA
crash <- na.omit(crash_seg)

###removing some aadt with no observations
crash_seg$AADT[crash_seg$AADT==0] <- NA
crash_seg <- na.omit(crash_seg)
summary(crash_seg$AADT)
summary(crash_seg$CRASH_ALL)

## For plotting the histogram
myhist <- hist(crash_seg$CRASH_ALL,include.lowest = T,right=F, las=1, col="grey", xlab="All-crashes",
               main = "Histogram of all the crashes")
## Zooming in on the hist
myhist1 <- hist(crash_seg$CRASH_ALL,include.lowest = T,ylim=c(0,800),right=F, las=1, col="grey", xlab="All-crashes",
               main = "Zoomed Histogram of all the crashes")
myhist2 <- hist(crash_seg$CRASH_ALL,include.lowest = T,ylim=c(0,200),right=F, las=1, col="grey", xlab="All-crashes",
                main = "Zoomed Histogram of all the crashes")

### Poisson regression model

mymod <- glm(CRASH_ALL ~ offset(log(DIST))+ AADT + SPEED_LIMIT + LANES_TOTAL, data = crash_seg, family=poisson)
summary(mymod)

100 * (exp(mymod$coefficients)-1)
##Goodness of fit
library("pscl")
pR2(mymod)

###re-estimation of model
mymod1 <- glm(CRASH_ALL ~ offset(log(DIST)) + log(AADT)   + SPEED_LIMIT + LANES_TOTAL, data = crash_seg, family=poisson)
summary(mymod1)

## interpredted parameter estimates for the non-logged IVs 
100 * (exp(mymod1$coefficients[3:4])-1)

##Godness of fit
pR2(mymod1)

###Negative binomial regression
library("MASS") 

Nbmod <-  glm.nb(CRASH_ALL ~ offset(log(DIST))+ log(AADT) +  SPEED_LIMIT + LANES_TOTAL, data = crash_seg) #control=glm.control(maxit=50)) 
summary(Nbmod)
100 * (exp(Nbmod$coefficients[3:4])-1)
pR2(Nbmod)

###Checking for mean and var
mean(crash_seg$CRASH_ALL)
var(crash_seg$CRASH_ALL)

##checking of the overdispersion
library("AER")
dispersiontest(mymod1,trafo = 1)

odTest(Nbmod)

###Estimating a zero inflated model
###Zero inflated neagtive binomial model
ZNbmod <- zeroinfl(CRASH_ALL ~  offset(log(DIST)) + log(AADT)  + SPEED_LIMIT + LANES_TOTAL |1, data = crash_seg, dist="negbin")
summary(ZNbmod)
100*(exp(ZNbmod$coefficients$count[3:4])-1)

###Inspection for zero-inflation
vuong(mymod1,ZNbmod)
vuong(Nbmod,ZNbmod)

####Godness of fit test
Null <- update(ZNbmod,.~ 1)

##Mc faddens R-square
1-logLik(ZNbmod)/logLik(Null)

#Number of zeros in DV
zero <- table(crash_seg$CRASH_ALL==0);zero
prop.table(zero)
###The end 

