##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Safety project

#The data loaded are processesd from excel and ARCGIS, some of it may be output of r analyzed in gis and again brought back as input.
library(readxl)
load("C:/Users/Niranjan.p/Box/MPC-603 Bicycle safety roundabouts/Data/Utah Crashes/crashes.RData")
crashes_indiv <- read_excel("Gis_crash_individuals.xls")
roundabout <- read_excel("Gis_roundabout.xls")
Before_after2010 <- read_excel("Final before after.xls")


##summary(crashes)

###Subsetting the crashes related to roundabout
crash <- subset(crashes,crashes$Roadway.Junction.Feature=="Roundabout")

###Removing the columns we might not require
crash <- crash[-c(1,2,9,10,11,13,14,15,16,17,21,22,23,24,25,26,27,28,30,39,40,42,45,50,51,52,53,54,55,57,58,59,60,61,62,63,64,65,66,
                  72,73,77,78,80,88,89,93,94,97,98,100,101,102,103,107,108,109,110,111,112,113,114,120,122,123,124,126,127,128,129,
                  134,136)]
###Further removal of the colums we will not require
crash <- crash[-c(7,8,9,10,11,12,14,21,22,23,36,41,44,51,49,53,56)]
crash <- crash[-c(2,7,10,11,12,14,18,19,20,21,22,23,26,33,36,40)]

###Checking differnt values to understand and further manipulate data
table(crash$Severity==crash$Severity.1)
table(crash$Day.of.Week)
table(crash$Manner.of.Collision)
table(crash$Pedestrian.Involved)
table(crash$Traffic.Control.Device)
##Further truncating the not required colums and only three rural roundabout so remove urban and rural
crash <- crash[-c(2,8,16,24,26)]

###summarizing the cordinates
summary(crash$geometry)
str(crash$geometry)
### using the cSplit function for Seperating the geometry column into latitude and longitude
library(splitstackshape)
a <- cSplit(crash,"geometry", sep =",", direction ="wide",drop=T)
a <- cSplit(a,"geometry_1", sep ="[", direction ="wide",drop=T)
a <- cSplit(a,"geometry_2", sep ="]", direction ="wide",drop=T)
a <- a[,-c(26,27)]

###Changing column name
colnames(a)[colnames(a)=="geometry_1_2"] <- "Longitude"
colnames(a)[colnames(a)=="geometry_2_1"] <- "Lattitude"

###There are some data with latitude less than 0 based on their location descrption we can chnage those value
a$Lattitude[a$City=="NORTH LOGAN"] <- 41.764880
a$Longitude[a$City=="NORTH LOGAN"] <- -111.8286565
a$Lattitude[a$City=="LOGAN" & a$Crash.ID==11168397] <- 41.740735
a$Longitude[a$City=="LOGAN" & a$Crash.ID==11168397] <- -111.819326
a$Lattitude[a$City=="PROVIDENCE" & a$Crash.ID==11171266] <- 41.710123
a$Longitude[a$City=="PROVIDENCE" & a$Crash.ID==11171266] <- -111.829151
a$Lattitude[a$City=="FARMINGTON" & a$Crash.ID==11146557] <- 40.98214
a$Longitude[a$City=="FARMINGTON" & a$Crash.ID==11146557] <- -111.911343

###Exporting the data to do further manipulation in Arc GIS
#write.csv(a,"crashdata_after_roundabout.csv")

####Furthermore for the befor analysis similar data is required
###Subsetting intersection related 
crash1 <- subset(crashes,crashes$Intersection.Related==T)
crash1 <- crash1[-c(1,2,9,10,11,13,14,15,16,17,21,22,23,24,25,26,27,28,30,39,40,42,45,50,51,52,53,54,55,57,58,59,60,61,62,63,64,65,66,
                  72,73,77,78,80,88,89,93,94,97,98,100,101,102,103,107,108,109,110,111,112,113,114,120,122,123,124,126,127,128,129,
                  134,136)]
crash1 <- crash1[-c(7,8,9,10,11,12,14,21,22,23,36,41,44,51,49,53,56)]
crash1 <- crash1[-c(2,7,10,11,12,14,18,19,20,21,22,23,26,33,36,40)]
crash1 <- crash1[-c(2,8,16,24,26)]
b <- cSplit(crash1,"geometry", sep =",", direction ="wide",drop=T)
b <- cSplit(b,"geometry_1", sep ="[", direction ="wide",drop=T)
b <- cSplit(b,"geometry_2", sep ="]", direction ="wide",drop=T)
b <- b[,-c(26,27)]
colnames(b)[colnames(b)=="geometry_1_2"] <- "Longitude"
colnames(b)[colnames(b)=="geometry_2_1"] <- "Lattitude"

###To further edit the data in ArcMap
### write.csv(b,"all-crashes.csv")

####Further processing of the crashes to relate to individual roundabout was done using the gis
#### The output of gis was edited for chnaging the name of header and stuff and will be brought back

###the data required is already loaded

###Charcaterstics of roundabout 
table(roundabout$Approach)
table(roundabout$Lane_type)

###Looking at the google map and editing the Unknown in lane typeof roundabouts
roundabout$Lane_type[roundabout$Object_ID==51] <- "Single-Lane"
roundabout$Lane_type[roundabout$Object_ID==36] <- "Single-Lane"
roundabout$Lane_type[roundabout$Object_ID==30] <- "Single-Lane"
roundabout$Lane_type[roundabout$Object_ID==29] <- "Single-Lane"


###looking at crash data
summary(crashes_indiv)

###Changing some column into factots
crashes_indiv$Bicyclist_ <- as.factor(crashes_indiv$Bicyclist_)
crashes_indiv$Severity <- as.factor(crashes_indiv$Severity)
crashes_indiv$Work_Zone <- as.factor(crashes_indiv$Work_Zone)
crashes_indiv$Weather_Condition<- as.factor(crashes_indiv$Weather_Condition)
crashes_indiv$Pedestrian<- as.factor(crashes_indiv$Pedestrian)
crashes_indiv$Motorcycle <- as.factor(crashes_indiv$Motorcycle)
crashes_indiv$Manner_of_collision <- as.factor(crashes_indiv$Manner_of_collision)
crashes_indiv$Light_Cond<- as.factor(crashes_indiv$Light_Cond)
crashes_indiv$Roadway_Surface<- as.factor(crashes_indiv$Roadway_Surface)
crashes_indiv$`Collision with fixed object`<- as.factor(crashes_indiv$`Collision with fixed object`)
crashes_indiv$aadt <- as.numeric(crashes_indiv$aadt)
crashes_indiv$Estimated_ <- as.numeric(crashes_indiv$Estimated_)
sd(crashes_indiv$Estimated_)
sd(crashes_indiv$aadt)

####Counting the number of crashes from two seperate files ###Assingning crash to respective roundabout

roundabout$crashes <-0


for(i in 1:nrow(roundabout)) {
  e = 0
  for ( j in 1:nrow(crashes_indiv)){
    if (roundabout$Object_ID[i]==crashes_indiv$roundabout[j]){
      e = e + 1
    }
    else {
      e = e + 0
    }
  } 
  roundabout$crashes[i]=e
}

rm(e);rm(i);rm(j)

###lets provide the year of crash
str(crashes_indiv$`Crash Date`)
crashes_indiv$Year <- format(as.POSIXct(crashes_indiv$`Crash Date`),format = "%Y")

# First lets remove data from the year 2019 
crashes_indiv1 <- subset(crashes_indiv, crashes_indiv$Year != 2019)
####Now seperate the number of crashes for the year after construction
roundabout$after <- 0

for(i in 1:nrow(roundabout)) {
  f = 0
  for ( j in 1:nrow(crashes_indiv1)){
    if (roundabout$Object_ID[i]==crashes_indiv1$roundabout[j] && roundabout$Year_completed[i]<crashes_indiv1$Year[j]){
      f = f + 1
    }
    else {
      f = f + 0
    }
  } 
  roundabout$after[i]=f
}
rm(f);rm(i);rm(j)

##Number of crashes related to roundabout
sum(roundabout$crashes)
sum(roundabout$after)


###Again we have calculated the crashes at roundabout constructed after 2010 (including before and after, in gis)
###Importing that file was already done in first step


###Creating a subset or roundabout only after 2010
roundabout_2010 <- subset(roundabout,roundabout$Year_completed >= 2010)

###Seperating year from the crash data
str(Before_after2010$Crash_Date)
Before_after2010$Year <- substring(Before_after2010$Crash_Date,1,4)
str(Before_after2010$Year)

###Assinging the number of crashes befor the construction of roundabout
roundabout_2010$Before <- 0

for(i in 1:nrow(roundabout_2010)) {
  f = 0
  for ( j in 1:nrow(Before_after2010)){
    if (roundabout_2010$Object_ID[i]==Before_after2010$roundabout[j] && roundabout_2010$Year_completed[i]>Before_after2010$Year[j]){
      f = f + 1
    }
    else {
      f = f + 0
    }
  } 
  roundabout_2010$Before[i]=f
}
rm(f);rm(i);rm(j)



###Removing roundabout constructed in 2010 and 2018
roundabout_2010 <- subset(roundabout_2010, roundabout_2010$Year_completed != 2010 & roundabout_2010$Year_completed != 2018)

###Some check was done to see ifd there were any data remaining not provided laltitude and longitude with the name of intersection
# ###There are crashes with no latitude and longitude we need to filter those data too
# ###Check at some suspicious roundabout only
# Nadata <- b[is.na(b$Lattitude),]
# 
# Nadata$check <- grepl("1300 E",Nadata$Location.Description)
# Nadata1 <- subset(Nadata, Nadata$check=="TRUE")
# Nadata1$check <- grepl("400 S",Nadata1$Location.Description)
# Nadata1 <- subset(Nadata1, Nadata1$check=="TRUE")
# 
# ###Check doesnot give new data furter check in whole data
# crashes$check <- grepl("1300 E",crashes$Location.Description,fixed=T)
# check1 <- subset(crashes, crashes$check=="TRUE")
# check1$check <- grepl("400 S",check1$Location.Description, fixed=T)
# check1 <- subset(check1, check1$check=="TRUE")

####looks like we have not missed that much data

###Roundabout 79 and 88, there was nothing previously
roundabout_2010 <- roundabout_2010[-c(6,12),]

#### check after and before total
sum(roundabout_2010$Before)
sum(roundabout_2010$after)

###The severity of the injury is categorized in four types, possible inhjury, minor injury and serious injury proportion would be checked
injurycrash_after <- subset(crashes_indiv1,crashes_indiv1$Severity=="Possible Injury" | crashes_indiv1$Severity=="Serious Injury" |
                              crashes_indiv1$Severity=="Minor Injury")
injurycrash_before <- subset(Before_after2010,Before_after2010$Severity=="Possible Injury" | Before_after2010$Severity=="Serious Injury" |
                               Before_after2010$Severity== "Minor Injury")


####Assingning the injury crashes to roundabout for injury crashes before and after
roundabout_2010$injury_a <- 0

for(i in 1:nrow(roundabout_2010)) {
  f = 0
  for ( j in 1:nrow(injurycrash_after)){
    if (roundabout_2010$Object_ID[i]==injurycrash_after$roundabout[j] && roundabout_2010$Year_completed[i]<injurycrash_after$Year[j]){
      f = f + 1
    }
    else {
      f = f + 0
    }
  } 
  roundabout_2010$injury_a[i]=f
}
rm(f);rm(i);rm(j)

roundabout_2010$injury_b <- 0
for(i in 1:nrow(roundabout_2010)) {
  f = 0
  for ( j in 1:nrow(injurycrash_before)){
    if (roundabout_2010$Object_ID[i]==injurycrash_before$roundabout[j] && roundabout_2010$Year_completed[i]>injurycrash_before$Year[j]){
      f = f + 1
    }
    else {
      f = f + 0
    }
  } 
  roundabout_2010$injury_b[i]=f
}
rm(f);rm(i);rm(j)

###Calculating the proportion of before and after injury crashes
roundabout_2010$B_prop <- ifelse(roundabout_2010$Before==0,0,roundabout_2010$injury_b/roundabout_2010$Before)
roundabout_2010$A_prop <- ifelse(roundabout_2010$after==0,0,roundabout_2010$injury_a/roundabout_2010$after)

###calculate difference in porportion
roundabout_2010$AB_prop <- roundabout_2010$A_prop - roundabout_2010$B_prop
mean(roundabout_2010$AB_prop)
mean(roundabout_2010$B_prop)
mean(roundabout_2010$A_prop)
wilcox.test(roundabout_2010$AB_prop)

sum(roundabout_2010$injury_b)
sum(roundabout_2010$injury_a)

###next we want to do a ordered logit model to look at the the serious injury incidents related to roundabout
##removing not needed colum from the data frame
ordered <- subset(crash, select=-c(aadt,City,Crash.Date.Time,Crash.ID,geometry,Location.Description,Milepoint,Virtual.Navigator,
                                   Estimated.Travel.Speeds))

###Splitting the speed of two vehicles
##order<- cSplit(ordered,"Estimated.Travel.Speeds", sep =",", direction ="wide",drop=T)
##order$Estimated.Travel.Speeds_1 <- gsub("\\[","",order$Estimated.Travel.Speeds_1)

#renaming the column to make it easy for analysis
names(ordered)[1]<- "Bike"
names(ordered)[2]<- "Fix_objcoll"
names(ordered)[3]<- "Vehicle1_speed"
names(ordered)[4]<- "Bike_lane"
names(ordered)[5]<- "Horz_alg"
names(ordered)[6]<- "Light"
names(ordered)[7]<- "Manner"
names(ordered)[8]<- "Motorcycle"
names(ordered)[9]<- "pedesterian"
names(ordered)[10]<- "Posted_speed"
names(ordered)[11]<- "Surface_cond"
names(ordered)[12]<- "Severity"
names(ordered)[13]<- "Traffic_control"
names(ordered)[14]<- "Vehicles_no"
names(ordered)[15]<- "Ver_algn"
names(ordered)[16]<- "Weather"
names(ordered)[17]<- "Work_zone"

###converting variable type where needed
ordered$Light <- as.factor(ordered$Light)
ordered$Manner <- as.factor(ordered$Manner)
ordered$Surface_cond <- as.factor(ordered$Surface_cond)
ordered$Traffic_control <- as.factor(ordered$Traffic_control)
ordered$Ver_algn <- as.factor(ordered$Ver_algn)
ordered$Weather <- as.factor(ordered$Weather)
###relevelling and reducing levels
str(ordered)
summary(ordered$Severity)
levels(ordered$Light) <- c("Dark_Light","Dark_NoLight","Unknown","Dawn-dusk","Daylight","Dawn-dusk","Unknown")
levels(ordered$Manner) <- c("Angle","Front to Rear","Headon and other","Parked Vehicle","Headon and other","Headon and other",
                            "Headon and other","Sideswip_same direction","Single vehicle")
levels(ordered$Surface_cond) <- c("Dry","Ice","other_unknown","other_unknown","other_unknown","slush",
                            "snow","other_unknown","other_unknown","Wet")
levels(ordered$Traffic_control) <- c("other_unknown","Control signal","None","other_unknown","other_unknown","other_unknown",
                                  "stop","Control signal","other_unknown","Warning","Yield")
levels(ordered$Weather) <- c("Blowing","Blowing","clear","cloudy","fog-smog","rain",
                                     "other_unknown","other_unknown","Snow","other_unknown")
###reodrding the level
ordered$Light <- relevel(ordered$Light,"Daylight")
ordered$Traffic_control <- relevel(ordered$Traffic_control,"None")
ordered$Weather <- relevel(ordered$Weather,"clear")

###removing NA in data

ordered <- na.omit(ordered)
summary(ordered$Severity)

###There is no fatal injury 
levels(ordered$Severity) <- c("No Injury","Possible Injury","Minor Injury","Serious Injury","Serious Injury")

##The name matches with function so rename
mod_data <- ordered

library("MASS")
library("lmtest")
library("pscl")

###ordered logit model
mymod <- polr(Severity ~ Bike + Fix_objcoll + Vehicle1_speed + Bike_lane + Horz_alg + Light + Manner + Motorcycle + pedesterian + 
                Posted_speed + Surface_cond + Traffic_control + Vehicles_no + Ver_algn + Weather + Work_zone,data=mod_data, 
              method = "logistic")
summary(mymod)
coeftest(mymod)
exp(mymod$coefficients)
100*(exp(mymod$coefficients)-1)

# Pseudo-R^2
pR2(mymod)

summary(ordered$Severity)


####the end #######

####Function to calculate percentage ###Not for submission
a$number <- 0

###loop
for(i in 1:45) {
  a$number[i] <- readline(promp = "Enter number")
  if (a$number[i]==100){
    break
  }
  a$number <- as.integer(a$number)
  a$number[i] <- (a$number[i]/2226) * 100 
  print(a$number[i])
  print("Enter 100 to end")
  
}

