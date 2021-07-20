##################################################
#CEE 5255/6255 Transportation Data and Safety Analysis
#Utah State University
#Niranjan Poudel(Niranjan111@hotmail.com)
#Lab 6


###loading the required dataset
load("C:/Users/Niranjan.p/Downloads/smdata.Rdata")
mydfw <- smdata

###removing observation with no mode choice
mydfw1 <- mydfw[is.na(mydfw$CHOICE)==F,]

###Looking at the choices 

mytab<- table(mydfw$CHOICE); mytab
myptab<-(prop.table(mytab))*100; myptab
bp<- barplot(myptab,las=1, main = "Percentages of commute mode choices ", xlab="Percentage", horiz=TRUE, 
             xlim=c(0,100))

###keeping the variable with the car choice
mydfw2 <- mydfw[mydfw$AV_CAR == 1,]

###Estimating a binary logistic model
mydfw2$LUGGAGE <- as.factor(mydfw2$LUGGAGE)
mymod <- glm(I(CHOICE=="CAR") ~ TT_CAR + CO_CAR + AGE + INCOME + LUGGAGE, data= mydfw2, family =binomial);summary(mymod)

# Odds ratios
exp(mymod$coefficients)

# Percentage changes in odds
100*(exp(mymod$coefficients)-1)

library("pscl")

# Pseudo-R^2
pR2(mymod)

###libary for the next part of assignment
library(mlogit)

####Reshaping my original wide data into long format, and removing modal alternatives that are not available
mydfl <- mlogit.data(mydfw,choice="CHOICE", shape = "wide", varying = 17:31, sep="_")
mydfl <- mydfl[mydfl$AV==1,]

###Frequency table of the mode chosen
mytab <- table(mydfl$CHOICE, mydfl$alt);mytab

#proportion table
myptab <- prop.table(mytab, margin=2); myptab

# Bar chart
par(mar=c(5,5,8,2)+0.3)
barplot(mytab, horiz=T, las=1, xlab="Frequency", main="Frequency of commute mode choices", 
        legend.text = row.names(mytab), args.legend = list(title="Mode chosen?", horiz=F),xlim=c(0,14000))


##estimating multinomial logit model
mydfl$LUGGAGE <- as.factor(mydfl$LUGGAGE)
mymod1 <- mlogit(CHOICE ~ TT + CO |AGE + INCOME + LUGGAGE | 0, data = mydfl, reflevel = "CAR"); summary(mymod1)


# Relative risk ratios
exp(mymod1$coefficients)

# Percentage changes in relative risk
100*(exp(mymod1$coefficients)-1)

# Pseudo-R^2
summary(mymod1)$mfR2

# Value of time
60 * (mymod1$coefficients[names(mymod1$coefficients)=="TT"]/mymod1$coefficients[names(mymod1$coefficients)=="CO"])




