##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 10


#Loading the crash data and aadt data

load("C:/Users/Niranjan.p/Downloads/crashdata.RData")

##library for gis data
library("sp")
library("rgdal")
load("C:/Users/Niranjan.p/Downloads/aadtshp.RData")

###Extracting AADT data.frame
aadtdata <- aadtshp@data

summary(crashdata$CRASH_SEVERITY_ID)


####Assignment part 1 creating a function
###SPF rural forfour-leg signalized intersections 

SPF <- function(crashtype = "all",AADT_maj,AADT_min,AADT_total) {
  if (AADT_maj <0 | AADT_maj >43500 ){warning("AADT_major should be between 0 and 43,500, the results may be unreliable")}
  if (AADT_min <0 | AADT_min >23000 ){warning("AADT_major should be between 0 and 23,000, the results may be unreliable")}
  if (crashtype=="all"){
    N <- exp(-7.182 + 0.722*log(AADT_maj) + 0.337*log(AADT_min))
  } else if (crashtype=="kabc"){
    N <- exp(-6.393 + 0.638*log(AADT_maj) + 0.232*log(AADT_min))
  } else if (crashtype=="kab") {
    N <- (-12.011 + 1.279 *log(AADT_total))
  } else {stop("Please specify one of 'all or 'kabc' or 'kab' crashtype.")
  }
  return(N) ## the diffent types of collision like head-on and so on is not specified
}

###Checking if the function above works
SPF(crashtype = "all",44400,300, 0)

####End of assignment part 1

###Subsetting crash and aadt data for 5 intersections
###First subsetting the required roads

datacrash <- subset(crashdata,crashdata$ROUTE=="30" | crashdata$ROUTE=="91" | crashdata$ROUTE =="56" | crashdata$ROUTE =="130" )
dataadt <- subset(aadtdata, aadtdata$RT_NUM == "30" | aadtdata$RT_NUM == "91" | aadtdata$RT_NUM == "56" | aadtdata$RT_NUM == "130")

# Create intersection data
Newdat <- data.frame(No = c(1,2,3,4,5))
Newdat[,c("Crashmaj","Crashmin1","Crashmin2","Totalcrash")] <- 0
Newdat$Major_route <- c("SR-30","US91","SR-56","SR-130","SR-130")
Newdat$Mpmajor <- c(91.901,40.004,59.004,1.469,6.513)
Newdat$Min1 <- c("SR-13","SR-142", "FA-1753", "FA-1758","FA-1780")
Newdat$Mp1 <- c(25.250,17.353,2.805,0,0.987)
Newdat$Min2 <- c(NA,NA,NA,"FA-1766","FA-1788")
Newdat$Mp2 <- c(NA,NA,NA,0,0)
Newdat[,c("Vmaj15", "Vmaj16", "Vmaj17", "Vmaj18")] <- 0
Newdat[,c("Vmin15", "Vmin16", "Vmin17", "Vmin18")] <- 0
###Subsetting major crashes
for (i in 1:nrow(Newdat)) {
  if(i==1){
    tcr <- subset(datacrash,datacrash$ROUTE=="30" &(datacrash$MILEPOINT >= (Newdat$Mpmajor[i]-0.1) & 
                                                      datacrash$MILEPOINT <= (Newdat$Mpmajor[i]+0.1)))
    tcr <- subset(tcr,tcr$INTERSECTION_RELATED==T)
    Newdat$Crashmaj[i] <- Newdat$Crashmaj[i] + nrow(tcr)
    rm(tcr)
  } else if (i==2) {
    tcr <- subset(datacrash,datacrash$ROUTE=="91" &(datacrash$MILEPOINT >= (Newdat$Mpmajor[i]-0.1) & 
                                                      datacrash$MILEPOINT <= (Newdat$Mpmajor[i]+0.1)))
    tcr <- subset(tcr,tcr$INTERSECTION_RELATED==T)
    Newdat$Crashmaj[i] <- Newdat$Crashmaj[i] + nrow(tcr)
    rm(tcr)
  } else if (i==3){
    tcr <- subset(datacrash,datacrash$ROUTE=="56" &(datacrash$MILEPOINT >= (Newdat$Mpmajor[i]-0.1) & 
                                                      datacrash$MILEPOINT <= (Newdat$Mpmajor[i]+0.1)))
    tcr <- subset(tcr,tcr$INTERSECTION_RELATED==T)
    Newdat$Crashmaj[i] <- Newdat$Crashmaj[i] + nrow(tcr)
    rm(tcr)
  } else if (i==4) {
    tcr <- subset(datacrash,datacrash$ROUTE=="130" &(datacrash$MILEPOINT >= (Newdat$Mpmajor[i]-0.1) & 
                                                      datacrash$MILEPOINT <= (Newdat$Mpmajor[i]+0.1)))
    tcr <- subset(tcr,tcr$INTERSECTION_RELATED==T)
    Newdat$Crashmaj[i] <- Newdat$Crashmaj[i] + nrow(tcr)
    rm(tcr)
  } else if (i==5){
    tcr <- subset(datacrash,datacrash$ROUTE=="130" &(datacrash$MILEPOINT >= (Newdat$Mpmajor[i]-0.1) & 
                                                       datacrash$MILEPOINT <= (Newdat$Mpmajor[i]+0.1)))
    tcr <- subset(tcr,tcr$INTERSECTION_RELATED==T)
    Newdat$Crashmaj[i] <- Newdat$Crashmaj[i] + nrow(tcr)
    rm(tcr)
  }

}; rm(i)
  
###Subsetting for minor crashes where there is only one type of minor

###First intersection has only one type minor
tcr <- subset(crashdata,crashdata$ROUTE=="13" & crashdata$MILEPOINT >= (25.250 -0.1) &
                crashdata$MILEPOINT <= (25.250 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin1[1] <- Newdat$Crashmin1[1] + nrow(tcr) 
rm(tcr)

###Second intersection has one type of minor
tcr <- subset(crashdata,crashdata$ROUTE=="142" & crashdata$MILEPOINT >= (17.353 -0.1) &
                crashdata$MILEPOINT <= (17.353 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin1[2] <- Newdat$Crashmin1[2] + nrow(tcr) 
rm(tcr)

##3Third intersection also has one type of minor
tcr <- subset(crashdata,crashdata$ROUTE=="1753" & crashdata$MILEPOINT >= (2.805 -0.1) &
                crashdata$MILEPOINT <= (2.805 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin1[3] <- Newdat$Crashmin1[3] + nrow(tcr) 
rm(tcr)

###Fourth intersection has two type of minor

###For first one 
tcr <- subset(crashdata,crashdata$ROUTE=="1758" & crashdata$MILEPOINT >= (0.00 -0.1) &
                crashdata$MILEPOINT <= (0.00 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin1[4] <- Newdat$Crashmin1[4] + nrow(tcr) 
rm(tcr)

###For the second one
tcr <- subset(crashdata,crashdata$ROUTE=="1766" & crashdata$MILEPOINT >= (0.00 -0.1) &
                crashdata$MILEPOINT <= (0.00 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin2[4] <- Newdat$Crashmin2[4] + nrow(tcr) 
rm(tcr)

###For the fifth intersection with two types of minor

###For first minor
tcr <- subset(crashdata,crashdata$ROUTE=="1780" & crashdata$MILEPOINT >= (0.987 -0.1) &
                crashdata$MILEPOINT <= (0.987 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin1[5] <- Newdat$Crashmin1[5] + nrow(tcr) 
rm(tcr)

###For the second minor
tcr <- subset(crashdata,crashdata$ROUTE=="1788" & crashdata$MILEPOINT >= (0 -0.1) &
                crashdata$MILEPOINT <= (0 + 0.1) & crashdata$INTERSECTION_RELATED==T)
Newdat$Crashmin2[5] <- Newdat$Crashmin2[5] + nrow(tcr) 
rm(tcr)

###Summing crashes for to get total crashes
Newdat$Totalcrash <- Newdat$Crashmaj + Newdat$Crashmin1 + Newdat$Crashmin2

###Susetting traffic major
###For 1st intersection
ttv <- subset(dataadt,dataadt$RT_NUM=="30" & dataadt$MP_END >= (91.901 - 0.001) & dataadt$MP_BEG <= (91.901 + 0.001))
Newdat$Vmaj15[1] <- max(Newdat$Vmaj15[1] + ttv$AADT2015)
Newdat$Vmaj16[1] <- max(Newdat$Vmaj16[1] + ttv$AADT2016)
Newdat$Vmaj17[1] <- max(Newdat$Vmaj17[1] + ttv$AADT2017)
Newdat$Vmaj18[1] <- max(Newdat$Vmaj18[1] + ttv$AADT2017)
rm(ttv)

#For 2nd intersection
ttv <- subset(dataadt,dataadt$RT_NUM=="91" & dataadt$MP_END >= (40.004 - 0.001) & dataadt$MP_BEG <= (40.004 + 0.001))
Newdat$Vmaj15[2] <- max(Newdat$Vmaj15[2] + ttv$AADT2015)
Newdat$Vmaj16[2] <- max(Newdat$Vmaj16[2] + ttv$AADT2016)
Newdat$Vmaj17[2] <- max(Newdat$Vmaj17[2] + ttv$AADT2017)
Newdat$Vmaj18[2] <- max(Newdat$Vmaj18[2] + ttv$AADT2017)
rm(ttv)

###For 3rd intesection
ttv <- subset(dataadt,dataadt$RT_NUM=="56" & dataadt$MP_END >= (59.004 - 0.001) & dataadt$MP_BEG <= (59.004 + 0.001))
Newdat$Vmaj15[3] <- max(Newdat$Vmaj15[3] + ttv$AADT2015)
Newdat$Vmaj16[3] <- max(Newdat$Vmaj16[3] + ttv$AADT2016)
Newdat$Vmaj17[3] <- max(Newdat$Vmaj17[3] + ttv$AADT2017)
Newdat$Vmaj18[3] <- max(Newdat$Vmaj18[3] + ttv$AADT2017)
rm(ttv)

###For the fourth intersection
ttv <- subset(dataadt,dataadt$RT_NUM=="130" & dataadt$MP_END >= (1.469 - 0.001) & dataadt$MP_BEG <= (1.469+ 0.001))
Newdat$Vmaj15[4] <- max(Newdat$Vmaj15[4] + ttv$AADT2015)
Newdat$Vmaj16[4] <- max(Newdat$Vmaj16[4] + ttv$AADT2016)
Newdat$Vmaj17[4] <- max(Newdat$Vmaj17[4] + ttv$AADT2017)
Newdat$Vmaj18[4] <- max(Newdat$Vmaj18[4] + ttv$AADT2017)
rm(ttv)

###For the fifth intersection
ttv <- subset(dataadt,dataadt$RT_NUM=="130" & dataadt$MP_END >= (6.513- 0.001) & dataadt$MP_BEG <= (6.513+ 0.001))
Newdat$Vmaj15[5] <- max(Newdat$Vmaj15[5] + ttv$AADT2015)
Newdat$Vmaj16[5] <- max(Newdat$Vmaj16[5] + ttv$AADT2016)
Newdat$Vmaj17[5] <- max(Newdat$Vmaj17[5] + ttv$AADT2017)
Newdat$Vmaj18[5] <- max(Newdat$Vmaj18[5] + ttv$AADT2017)
rm(ttv)

####Lets look for the minor
##First intersection
ttv <- subset(aadtdata,aadtdata$RT_NUM=="13" & aadtdata$MP_END >= (25.250- 0.001) & aadtdata$MP_BEG <= (25.250+ 0.001))
Newdat$Vmin15[1] <- max(Newdat$Vmin15[1] + ttv$AADT2015)
Newdat$Vmin16[1] <- max(Newdat$Vmin16[1] + ttv$AADT2016)
Newdat$Vmin17[1] <- max(Newdat$Vmin17[1] + ttv$AADT2017)
Newdat$Vmin18[1] <- max(Newdat$Vmin18[1] + ttv$AADT2017)
rm(ttv)

###Second intersection
ttv <- subset(aadtdata,aadtdata$RT_NUM=="142" & aadtdata$MP_END >=(17.353-0.001) & aadtdata$MP_BEG <= (17.353 + 0.001))
Newdat$Vmin15[2] <- max(Newdat$Vmin15[2] + ttv$AADT2015)
Newdat$Vmin16[2] <- max(Newdat$Vmin16[2] + ttv$AADT2016)
Newdat$Vmin17[2] <- max(Newdat$Vmin17[2] + ttv$AADT2017)
Newdat$Vmin18[2] <- max(Newdat$Vmin18[2] + ttv$AADT2017)
rm(ttv)

###Third intersection
ttv <- subset(aadtdata,aadtdata$RT_NUM=="1753" & aadtdata$MP_END >=(2.805-0.001) & aadtdata$MP_BEG <= (2.805 + 0.001))
Newdat$Vmin15[3] <- max(Newdat$Vmin15[3] + ttv$AADT2015)
Newdat$Vmin16[3] <- max(Newdat$Vmin16[3] + ttv$AADT2016)
Newdat$Vmin17[3] <- max(Newdat$Vmin17[3] + ttv$AADT2017)
Newdat$Vmin18[3] <- max(Newdat$Vmin18[3] + ttv$AADT2017)
rm(ttv)

###Fourth intersection 
###Two minor roads
ttv <- subset(aadtdata,aadtdata$RT_NUM=="1758" & aadtdata$MP_END >=(0-0.001) & aadtdata$MP_BEG <= (0 + 0.001))
ttv2 <- subset(aadtdata,aadtdata$RT_NUM=="1766" & aadtdata$MP_END >=(0-0.001) & aadtdata$MP_BEG <= (0 + 0.001))
###The count for second one is missing in data so go with only first one
Newdat$Vmin15[4] <- max(Newdat$Vmin15[4] + ttv$AADT2015)
Newdat$Vmin16[4] <- max(Newdat$Vmin16[4] + ttv$AADT2016)
Newdat$Vmin17[4] <- max(Newdat$Vmin17[4] + ttv$AADT2017)
Newdat$Vmin18[4] <- max(Newdat$Vmin18[4] + ttv$AADT2017)
rm(ttv)
rm(ttv2)
###Fifth intersection
###First minor
ttv <- subset(aadtdata,aadtdata$RT_NUM=="1780" & aadtdata$MP_END >=(0.987-0.001) & aadtdata$MP_BEG <= (0.987 + 0.001))
ttv2 <- subset(aadtdata,aadtdata$RT_NUM=="1788" & aadtdata$MP_END >=(0-0.001) & aadtdata$MP_BEG <= (0 + 0.001))

Newdat$Vmin15[5] <- max(Newdat$Vmin15[5] + ttv$AADT2015,Newdat$Vmin15[5] + ttv2$AADT2015)
Newdat$Vmin16[5] <- max(Newdat$Vmin16[5] + ttv$AADT2016,Newdat$Vmin16[5] + ttv2$AADT2016)
Newdat$Vmin17[5] <- max(Newdat$Vmin17[5] + ttv$AADT2017,Newdat$Vmin17[5] + ttv2$AADT2017)
Newdat$Vmin18[5] <- max(Newdat$Vmin18[5] + ttv$AADT2017,Newdat$Vmin18[5] + ttv2$AADT2017)
rm(ttv)
rm(ttv2)

###The total comule is not added as it can be used as the sum of maj and min volumes
###Final inspect
Newdat

###Applying SPF 
Newdat$SPF15 <- mapply(SPF, crashtype = "all",Newdat$Vmaj15,Newdat$Vmin15,Newdat$Vmaj15+Newdat$Vmin15)
Newdat$SPF16 <- mapply(SPF, crashtype = "all",Newdat$Vmaj16,Newdat$Vmin16,Newdat$Vmaj16+Newdat$Vmin16)
Newdat$SPF17 <- mapply(SPF, crashtype = "all",Newdat$Vmaj17,Newdat$Vmin17,Newdat$Vmaj17+Newdat$Vmin17)
Newdat$SPF18 <- mapply(SPF, crashtype = "all",Newdat$Vmaj18,Newdat$Vmin18,Newdat$Vmaj16+Newdat$Vmin18)
Newdat$SPF <- rowSums(Newdat[,c("SPF15","SPF16","SPF17","SPF18")])
Newdat


###CMF is not recommended for this method
###Applying EB method
##Overdispersion parameter k
Newdat$k <- c(0.277,0.277,0.277,0.277,0.277) ##All the intersections have base characterstics for all type of crashes

##weight
Newdat$w <- 1/(1 + Newdat$k * Newdat$SPF)

##expected
Newdat$Nexpec <- Newdat$w * Newdat$SPF + (1-Newdat$w)*Newdat$Totalcrash

###Inspect data
Newdat

Newdat[,c("Totalcrash","SPF","Nexpec")]


###The end