##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 10

## Loading the required crash data

load("C:/Users/Niranjan.p/Downloads/crashes_segments.RData")

##remove routes with low speeds amd missing AADT
mydfl <- subset(crashes_segments, DIST>0.001 & !is.na(AADT))

###Selecting the segments of US89 in Utah county
mydfl <- subset(mydfl, ROUTE =="0089P" & mydfl$MP_BEG>=250 & mydfl$MP_END <= 400)

###Further selscting the rural and two thru lanes samples
mydfl <- subset(mydfl, ROUTE=="0089P" & mydfl$AREA_TYPE=="Rural" & mydfl$LANES_THRU==2)

###Define befor and after periods
mydfl$PERIOD <- ifelse(mydfl$YEAR %in% c(2010:2014), "Before", ifelse(mydfl$YEAR %in% c(2016:2018),"After",NA))

###Reshaping from long to wide
mydfw <- unique(mydfl[,c("ROUTE","MP_BEG","MP_END","DIST")])


###SPF function for the site rural-two lane, two-ways segments
spf.R2L.seg <- function(AADT, L) {
  if (AADT < 0 | AADT > 17800) {
    warning("Value for AADT not between 0 and 17,800. Results may not be reliable.") 
  }
  N <- AADT * L * 365 * 10^(-6) * exp(-0.312)
  return(N)
}

###Rename the datasets
mydfw1 <- mydfw
mydfl1 <- mydfl

###Apply the spf function
mydfl1$Nspf <- mapply(FUN=spf.R2L.seg,AADT=mydfl1$AADT,L=mydfl1$DIST)

###Restrain related crashes
mydfl1$Nspf <- mydfl1$Nspf * 4.75/100

##CMF
mydfl1$Ncmf <- 1.00 * mydfl1$Nspf

###Sum across all befor and after years
mydfw1$B_Nobs <- 0
mydfw1$B_Npred <- 0
mydfw1$A_Nobs <- 0
mydfw1$A_Npred <- 0

for(i in 1 : nrow(mydfw1)) {
  temp <- subset(mydfl1,mydfl1$ROUTE==mydfw1$ROUTE[i] & mydfl1$MP_BEG ==mydfw1$MP_BEG[i] & mydfl1$MP_END==mydfw1$MP_END[i])
  tempB <- subset(temp, temp$PERIOD =="Before")
  tempA <- subset(temp,temp$PERIOD =="After")
  mydfw1$B_Nobs[i] <- sum(tempB$CRASH_SEATBELT)
  mydfw1$B_Npred[i] <- sum(tempB$Ncmf)
  mydfw1$A_Nobs[i] <- sum(tempA$CRASH_SEATBELT)
  mydfw1$A_Npred[i] <- sum(tempA$Ncmf)
  rm(temp,tempB,tempA)
};rm(i)

##3calculate the overdispersion parameter and weights
mydfw1$B_k <- 0.236/mydfw1$DIST
mydfw1$B_w <- 1/(1+mydfw1$B_k * mydfw1$B_Npred)

##Get expected in before period (using EB equation)
mydfw1$B_Nexp <- mydfw1$B_w * mydfw1$B_Npred + (1 - mydfw1$B_w) * mydfw1$B_Nobs

##Adjustment factor 
mydfw1$adj_r <- mydfw1$A_Npred / mydfw1$B_Npred

###Expected in after period
mydfw1$A_Nexp <- mydfw1$B_Nexp * mydfw1$adj_r

# Calculate treatment effectiveness
mydfw1$TE_OR <- mydfw1$A_Nobs / mydfw1$A_Nexp
mydfw1$TE_perc <- 100 * (mydfw1$TE_OR - 1)

# Calculate biased overall treatment effectiveness
TE_OR_bias <- sum(mydfw1$A_Nobs) / sum(mydfw1$A_Nexp)
TE_perc_bias <- 100 * (TE_OR_bias - 1)

# Calculate variance
var_Nexp <- sum(mydfw1$adj_r * mydfw1$adj_r * mydfw1$B_Nexp * (1 - mydfw1$B_w))

# Calculate unbiased overall treatment effectiveness
TE_OR <- TE_OR_bias / (1 + (var_Nexp / sum(mydfw1$A_Nexp)^2))
TE_perc <- 100 * (TE_OR - 1)

# Calculate variance
var_TE_OR <- (TE_OR_bias)^2 * ((1 / sum(mydfw1$A_Nobs)) + (var_Nexp / sum(mydfw1$A_Nexp)^2)) / (1 + (var_Nexp / sum(mydfw1$A_Nexp)^2))

# Calculate standard errors
se_TE_OR <- sqrt(var_TE_OR)
se_TE_perc <- 100 * se_TE_OR

# Assess statistical significance
TE_perc / se_TE_perc

2*pt(TE_perc / se_TE_perc, df=nrow(mydfw1), lower.tail=F)

####Same original data set
mydfl2 <- mydfl
mydfw2 <- mydfw

# Sum across all before years and after years
mydfw2$B_Nunr <- 0
mydfw2$B_Nall <- 0
mydfw2$A_Nunr <- 0
mydfw2$A_Nall <- 0

for (i in 1:nrow(mydfw2)) {
  temp <- subset(mydfl2, mydfl2$ROUTE==mydfw2$ROUTE[i] & mydfl2$MP_BEG==mydfw2$MP_BEG[i] & mydfl2$MP_END==mydfw2$MP_END[i])
  tempB <- subset(temp, temp$PERIOD=="Before")
  tempA <- subset(temp, temp$PERIOD=="After")
  mydfw2$B_Nunr[i] <- sum(tempB$CRASH_SEATBELT)
  mydfw2$B_Nall[i] <- sum(tempB$CRASH_ALL)
  mydfw2$A_Nunr[i] <- sum(tempA$CRASH_SEATBELT)
  mydfw2$A_Nall[i] <- sum(tempA$CRASH_ALL)
  rm(temp, tempB, tempA)
}; rm(i)

#calculate proportion
mydfw2$B_prop <- ifelse(mydfw2$B_Nall==0, 0, mydfw2$B_Nunr / mydfw2$B_Nall)
mydfw2$A_prop <- ifelse(mydfw2$A_Nall==0, 0, mydfw2$A_Nunr / mydfw2$A_Nall)

# Calculate difference in proportion
mydfw2$AB_prop <- mydfw2$A_prop - mydfw2$B_prop

# Calculate average difference
mean(mydfw2$AB_prop)

# Wilcoxan signed-rank test
wilcox.test(mydfw2$AB_prop)

###the end


