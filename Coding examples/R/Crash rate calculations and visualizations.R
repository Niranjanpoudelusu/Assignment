##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 8

###Loading the crash data
load("C:/Users/Niranjan.p/Downloads/crashdata.RData")

###Looking at the general nature of the data
str(crashdata)

###Filtering the data for the year 2015 to 2018 for the US-89 road in Utah county
crashdatasub <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crashdatasub <- subset(crashdatasub, crashdatasub$COUNTY_NAME=="UTAH" & crashdatasub$ROUTE=="89")

###Creating pie charet for the severity level

##first looking at the proportions
mytab <- table(crashdatasub$CRASH_SEVERITY_ID)
myptab <- prop.table(mytab)
mytab;myptab
###Pie chart
pie(myptab, labels=paste("(", round(100*myptab), "%)", sep=""),clockwise = T,radius =0.8,cex=0.8,las=3,
    main="Crash severities (number and percentage)", col=c(3,5,4,2,1))
###Changing label is tricky, requires to operate from other packages
legend("topright", legend=names(myptab), fill=c(3,5,4,2,1), ncol=1)

##Installing and loading packages to read shape files
#install.packages ("sp")
#install.packages("rgdal")

library("sp")
library("rgdal")

###Load AADT data which is already processed by professor

load("C:/Users/Niranjan.p/Downloads/aadtshp.RData")

###Subsetting the data fro Highway 89
aadtshpsub <- subset(aadtshp, aadtshp@data$RT_NUM=="89")

###Milepost along the highway to seperate county data
range(crashdatasub$MILEPOINT)[1]

aadtshpsub <- subset(aadtshpsub, aadtshpsub@data$MP_BEG >= range(crashdatasub$MILEPOINT)[1])
aadtshpsub <- subset(aadtshpsub, aadtshpsub@data$MP_END <= range(crashdatasub$MILEPOINT)[2])

###Creating the map of the Highway
#load package for color
library("RColorBrewer")

mycols <- brewer.pal(9,"YlGnBu")
mybins <- cut(aadtshpsub$AADT_2017/1000, 9)


plot(aadtshpsub, col=mycols[mybins], lwd=aadtshpsub$AADT_2017/1000, lend=1, ljoin=2, main="Map of traffic along US 89 in Utah County")
lines(aadtshpsub, col="grey")
legend("bottomright", legend=levels(mybins), fill=mycols, title="AADT (1000s)")

###Assigning sections to each crash
mybins <- sort(unique(c(aadtshpsub@data$MP_BEG, aadtshpsub@data$MP_END))); mybins
crashdatasub$SECTION <- cut(crashdatasub$MILEPOINT, breaks=mybins)

##3Creating datafreames with crashes by section
mytab <- table(crashdatasub$SECTION)
myptab  <- prop.table(mytab)
names(mytab) <- sort(aadtshpsub@data$MP_BEG)
mynewtab <- as.data.frame(mytab)
names(mynewtab) <- c("MP_BEG", "CRASHES")

##Merging two data with milepost
newdatasub <- merge(aadtshpsub, mynewtab, by="MP_BEG")

###Number of crashes in each section
newdatasub@data$CRASHES

###Each section lengths and adding as a new colum
newdatasub@data$LENGTH <- newdatasub@data$MP_END - newdatasub@data$MP_BEG

###Number of crashes per mile for each segments
newdatasub@data$CRASHESPERMI <- newdatasub@data$CRASHES / newdatasub@data$LENGTH

##Looking at above created data
newdatasub@data[order(newdatasub@data$MP_BEG),c("MP_BEG", "MP_END", "CRASHES", "LENGTH", "CRASHESPERMI", "AADT_2017")]

###Correlation between the number of crashes permile and AADT of each section
cor(newdatasub@data$CRASHESPERMI, newdatasub@data$AADT_2017)

##Scatter plot of AADT versus crash permile of each segment
plot(newdatasub@data$AADT_2017, newdatasub@data$CRASHESPERMI, las=1, lwd=2, pch=21, cex=2, bg="grey", 
     main="Traffic volumes versus crashes per mile", xlab="AADT", ylab="Crashes per mile", 
     xlim=c(0,max(newdatasub@data$AADT_2017)), ylim=c(0,max(newdatasub@data$CRASHESPERMI)))

###The data required for the second part of the assignment are already loaded just subsetting required
crash_seg <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_seg <- subset(crash_seg, crash_seg$COUNTY_NAME=="UTAH" & crash_seg$ROUTE=="89")
crash_seg <- subset(crash_seg, crash_seg$MILEPOINT > 328.555 & crash_seg$MILEPOINT < 328.847)
crash_seg <- subset(crash_seg, crash_seg$INTERSECTION_RELATED==F)
nrow(crash_seg)

###AADT links corresponding to this segment
aadt_seg <- subset(aadtshp@data, aadtshp@data$RT_NUM=="89")
aadt_seg <- subset(aadt_seg, aadt_seg$MP_BEG <= 328.555 & aadt_seg$MP_END >= 328.847)
aadt_seg$AADT_2017

##Calculating total crashes
C_seg <- nrow(crash_seg)
##Number of years
N_seg <- 4
#calc segments lengths
L_seg <- 328.847 - 328.555
###get daily volume (2017)
V_seg <- aadt_seg$AADT_2017

###Calculating the crash rate
cr_seg <- (100000000 * C_seg) / (365 * N_seg * V_seg * L_seg)
# summarize
paste0("Length:  ", L_seg, " mi"); paste0("Crashes: ", C_seg); paste0("Volume:  ", V_seg,
                                                                      " vpd"); paste0("Rate:    ", round(cr_seg,2), " crashes/100M-VMT")
##Next segment also
crash_seg1 <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_seg1 <- subset(crash_seg1, crash_seg1$COUNTY_NAME=="UTAH" & crash_seg1$ROUTE=="89")
crash_seg1 <- subset(crash_seg1, crash_seg1$MILEPOINT > 325.719 & crash_seg1$MILEPOINT < 326.232)
crash_seg1 <- subset(crash_seg1, crash_seg1$INTERSECTION_RELATED==F)
nrow(crash_seg1)
###AADT links corresponding to this segment
aadt_seg1 <- subset(aadtshp@data, aadtshp@data$RT_NUM=="89")
aadt_seg1 <- subset(aadt_seg1, aadt_seg1$MP_BEG <= 325.719 & aadt_seg1$MP_END >= 326.232)
aadt_seg1$AADT_2017

C_seg1 <- nrow(crash_seg1)
N_seg1 <- 4
L_seg1 <- 326.232 - 325.719
V_seg1 <- aadt_seg1$AADT_2017

###Calculating the crash rate
cr_seg1 <- (100000000 * C_seg1) / (365 * N_seg1 * V_seg1 * L_seg1)
# summarize
paste0("Length:  ", L_seg1, " mi"); paste0("Crashes: ", C_seg1); paste0("Volume:  ", V_seg1,
                                                                      " vpd"); paste0("Rate:    ", round(cr_seg1,2), " crashes/100M-VMT")

###Further more crash rate studies for intersection

##Alongg the major US89 road
crash_int_maj <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_int_maj <- subset(crash_int_maj, crash_int_maj$COUNTY_NAME=="UTAH" & crash_int_maj$ROUTE=="89")
crash_int_maj <- subset(crash_int_maj, crash_int_maj$MILEPOINT >= (328.847-0.1) & crash_int_maj$MILEPOINT <= (328.847+0.1))
crash_int_maj <- subset(crash_int_maj, crash_int_maj$INTERSECTION_RELATED==T)

##Along the minor FA3086 route
crash_int_min <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_int_min <- subset(crash_int_min, crash_int_min$COUNTY_NAME=="UTAH" & crash_int_min$ROUTE=="3086")
crash_int_min <- subset(crash_int_min, crash_int_min$MILEPOINT >= (0.378-0.1) & crash_int_min$MILEPOINT <= (0.378+0.1))
crash_int_min <- subset(crash_int_min, crash_int_min$INTERSECTION_RELATED==T)
nrow(crash_int_maj); nrow(crash_int_min)


####Subsetting AADT data for US 89
aadt_int_maj <- subset(aadtshp@data, aadtshp@data$RT_NUM=="89")
aadt_int_maj <- subset(aadt_int_maj, aadt_int_maj$MP_BEG <= (328.847+0.001) & aadt_int_maj$MP_END >= (328.847-0.001))

# subset AADT data for FA 3086
aadt_int_min <- subset(aadtshp@data, aadtshp@data$RT_NUM=="3086")
aadt_int_min <- subset(aadt_int_min, aadt_int_min$MP_BEG <= (0.378+0.001) & aadt_int_min$MP_END >= (0.378-0.001))
aadt_int_maj$AADT_2017; aadt_int_min$AADT_2017

###Calculating total crashes
C_int <- nrow(crash_int_maj) + nrow(crash_int_min)

##Number of years
N_seg <- 4

##calculating daily entering volume
V_int <- 0.50*aadt_int_maj$AADT_2017[1] + 0.50*aadt_int_maj$AADT_2017[2] +
  0.50*aadt_int_min$AADT_2017[1] + 0.50*aadt_int_min$AADT_2017[2]

### Crash rate 
# calc crash rate
cr_int <- (1000000 * C_int) / (365 * N_seg * V_int)
# summarize
paste0("Crashes: ", C_int); paste0("Volume:  ", V_int, " vpd"); paste0("Rate:    ", round(cr_int,2), " crashes/MEV")


###Same procedure for 2nd intersection
crash_int_maj1 <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_int_maj1 <- subset(crash_int_maj1, crash_int_maj1$COUNTY_NAME=="UTAH" & crash_int_maj1$ROUTE=="89")
crash_int_maj1 <- subset(crash_int_maj1, crash_int_maj1$MILEPOINT >= (325.719-0.1) & crash_int_maj1$MILEPOINT <= (325.719+0.1))
crash_int_maj1 <- subset(crash_int_maj1, crash_int_maj1$INTERSECTION_RELATED==T)

##There are two types of minor roads

crash_int_min1 <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_int_min1 <- subset(crash_int_min1, crash_int_min1$COUNTY_NAME=="UTAH" & crash_int_min1$ROUTE=="147")
crash_int_min1 <- subset(crash_int_min1, crash_int_min1$MILEPOINT >= (18.175-0.1) & crash_int_min1$MILEPOINT <= (18.175+0.1))
crash_int_min1 <- subset(crash_int_min1, crash_int_min1$INTERSECTION_RELATED==T)

crash_int_min2 <- crashdata[format(crashdata$CRASH_DATETIME, "%Y") %in% c(2015:2018),]
crash_int_min2 <- subset(crash_int_min2, crash_int_min2$COUNTY_NAME=="UTAH" & crash_int_min2$ROUTE=="3037")
crash_int_min2 <- subset(crash_int_min2, crash_int_min2$MILEPOINT >= (1.081-0.1) & crash_int_min2$MILEPOINT <= (1.081+0.1))
crash_int_min2 <- subset(crash_int_min2, crash_int_min2$INTERSECTION_RELATED==T)

nrow(crash_int_maj1); nrow(crash_int_min1);nrow(crash_int_min2)

###Subsetting aadt for all three types of roads
###US-89
aadt_int_maj1 <- subset(aadtshp@data, aadtshp@data$RT_NUM=="89")
aadt_int_maj1 <- subset(aadt_int_maj1, aadt_int_maj1$MP_BEG <= (325.719+0.001) & aadt_int_maj1$MP_END >= (325.719-0.001))

##SR147
aadt_int_min1 <- subset(aadtshp@data, aadtshp@data$RT_NUM=="147")
aadt_int_min1 <- subset(aadt_int_min1, aadt_int_min1$MP_BEG <= (18.175+0.001) & aadt_int_min1$MP_END >= (18.175-0.001))
aadt_int_maj1$AADT_2017; aadt_int_min1$AADT_2017

#####FA 3037
aadt_int_min2 <- subset(aadtshp@data, aadtshp@data$RT_NUM=="3037")
aadt_int_min2 <- subset(aadt_int_min2, aadt_int_min2$MP_BEG <= (1.081+0.001) & aadt_int_min2$MP_END >= (1.081-0.001))
aadt_int_min2$AADT_2017

###Calculating total crashes
C_int1 <- nrow(crash_int_maj1) + nrow(crash_int_min1) + nrow(crash_int_min2)

##Number of years
N_seg1 <- 4

##calculating daily entering volume
V_int1 <- 0.50*aadt_int_maj1$AADT_2017[1] + 0.50*aadt_int_maj1$AADT_2017[2] +
  0.50*aadt_int_min1$AADT_2017[1] + 0.50*aadt_int_min2$AADT_2017[1]

### Crash rate 
# calc crash rate
cr_int1 <- (1000000 * C_int1) / (365 * N_seg1 * V_int1)
# summarize
paste0("Crashes: ", C_int1); paste0("Volume:  ", V_int1, " vpd"); paste0("Rate:    ", round(cr_int1,2), " crashes/MEV")

######The end###
