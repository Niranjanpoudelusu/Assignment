# -*- coding: utf-8 -*-
"""
Created on Sun Jan 31 15:12:56 2021

@author: Niranjan.p
"""




import pandas as pd
import biogeme.database as db
import biogeme.biogeme as bio
import biogeme.models as models
import biogeme.messaging as msg
from biogeme.expressions import Beta, DefineVariable, bioDraws, \
    PanelLikelihoodTrajectory, MonteCarlo, log
    
# Read the data
df = pd.read_csv('Hetro_Imp_Sat.csv')
database=db.Database("Hetro_Imp_Sat",df)

database.panel("ID")
globals().update(database.variables)

## Next we define the parameters to be estimated

ASC_Current = Beta('ASC_Current', 0, None, None, 0)
B_Time = Beta('B_Time',-0.057, None, None, 0)
B_Cost = Beta('B_Cost', -0.305, None, None, 0)
B_Work = Beta('B_Work', -0.0038, None, None, 0)
B_Income = Beta('B_Income', 0.0429, None, None, 0)

### Standard deviation for  parameter
sigma_asc = Beta('sigma_asc', 0.25, None, None, 0)
sigma_time = Beta('sigma_time', 0.0599, None, None, 0)
sigma_cost = Beta('sigma_cost', 0.3, None, None, 0)
sigma_work = Beta('sigma_work', 0.0257, None, None, 0)
sigma_income = Beta('sigma_income', 0.117, None, None, 0)


### Next we define the heterogenity parameters to be estimated.

#Work
BCommute_NoWork = Beta('BCommute_NoWork',-0.0183,None,None,0)

BMODE_AutoPassengerWork = Beta('BMODE_AutoPassengerWork',0.0566,None,None,0)
BMODE_BikeWalkOtherWork = Beta('BMODE_BikeWalkOtherWork',0.0236,None,None,0)
BEduc_HSOrLessOtherWork = Beta('BEduc_HSOrLessOtherWork',0.0112,None,None,0)

BFuelCostWork = Beta('BFuelCostWork',-0.0275,None,None,0)
BFareCOSTWork = Beta('BFareCOSTWork',-0.021	,None,None,0)

BTCIMPWork = Beta('BTCIMPWork', -0.00349, None, None,0)


###Income

BWdaysIncome = Beta('BWdaysIncome',	-0.258,None,None,0)
BPrimarySourceIncomeIncome = Beta('BPrimarySourceIncomeIncome',-0.0493,None,None,0)
BCommute_NoIncome = Beta('BCommute_NoIncome',0.0636,None,None,0)

BHHINC_Less50Income = Beta('BHHINC_Less50Income',0.0793,None,None,0)
BHHINC_150PlusIncome = Beta('BHHINC_150PlusIncome',-0.0431,None,None,0)
BHHINC_MissingIncome = Beta('BHHINC_MissingIncome',0.426,None,None,0)

BMODE_AutoPassengerIncome = Beta('BMODE_AutoPassengerIncome',-0.0606,None,None,0)
BEduc_MasterIncome = Beta('BEduc_MasterIncome',-0.0658,None,None,0)

BLivToWork_RuralIncome = Beta('BLivToWork_RuralIncome',0.151,None,None,0)
BLivToWork_SubUrbIncome = Beta('BLivToWork_SubUrbIncome',0.0963,None,None,0)
BComOrWork_WorkIncome = Beta('BComOrWork_WorkIncome',0.0447,None,None,0) ### Missing in intial analysis

BTTImpIncome = Beta('BTTImpIncome', 0.0215, None,None, 0)
BWTimeImpIncome = Beta('BWTimeImpIncome', -0.0208, None, None, 0)
BWorkSatIncome = Beta('BWorkSatIncome', -0.028, None, None, 0 )

# Time

BFlexWhrsTime = Beta('BFlexWhrsTime',0.0179,None,None,0)
BRaceTime = Beta('BRaceTime',-0.0261,None,None,0)
BCommute_NoTime = Beta('BCommute_NoTime',-0.0386,None,None,0)

BHHINC_100To150Time = Beta('BHHINC_100To150Time',0.0226,None,None,0)

BMODE_AutoPassengerTime = Beta('BMODE_AutoPassengerTime',0.0478	,None,None,0)
BMODE_BikeWalkOtherTime = Beta('BMODE_BikeWalkOtherTime',0.0511,None,None,0)

BLivingPlace_UrbanTime = Beta('BLivingPlace_UrbanTime',0.0338,None,None,0)

BComOrWork_WorkTime = Beta('BComOrWork_WorkTime',0,None,None,0)

BWTimeImpTime = Beta('BWTimeImpTime', -0.0158, None, None,0)



##Cost
BAgeCost = Beta('BAgeCost',-0.499,None,None,0)
BPrimarySourceIncomeCost = Beta('BPrimarySourceIncomeCost',0.179,None,None,0)
BCommute_NoCost = Beta('BCommute_NoCost',-0.0969,None,None,0)

BCDaysCost = Beta('BCDaysCost',0.395,None,None,0) ### Might be correlated 
BHHINC_Less50Cost = Beta('BHHINC_Less50Cost',-0.13,None,None,0)
BGend_MaleCost = Beta('BGend_MaleCost',0.0966,None,None,0)

BMODE_BikeWalkOtherCost = Beta('BMODE_BikeWalkOtherCost',0.186,None,None,0)
BEduc_MasterCost = Beta('BEduc_MasterCost',0.136,None,None,0)

BParkingCOstCost = Beta('BParkingCOstCost',0.191,None,None,0)
BTollCostCost = Beta('BTollCostCost',0.237,None,None,0)

BLivToWork_SubCost = Beta('BLivToWork_SubCost',-0.116,None,None,0)
BComOrWork_WorkCost = Beta('BComOrWork_WorkCost',0.0447,None,None,0) ### Left in previous model
BIdealComTime_1To5Cost = Beta('BIdealComTime_1To5Cost',-0.188,None,None,0)
BIdealComTime_ZeroCost = Beta('BIdealComTime_ZeroCost',-0.239,None,None,0)

BIncImpCost = Beta('IncImpCost', 0.0517, None, None, 0)
BTCIMPCost = Beta('BTCIMPCost', -0.0424, None, None, 0)



#########################################################################################################################
################## Scaling of continious variable is needed. ###########################################################
########################################################################################################################

### Scaling the variables needed
Ages = DefineVariable('Ages', Age/100, database)
CDayss= DefineVariable('CDayss', CDays/10, database)
WDAYSs= DefineVariable('WDAYSs', WDAYS/10, database)
     
### Error terms normally distributed
Asc_RND = ASC_Current + sigma_asc *bioDraws('Asc_RND', 'NORMAL')

B_Work_RND = B_Work + sigma_work * bioDraws('B_Work_RND', 'NORMAL')  + BCommute_NoWork * Commute_No + \
     BMODE_AutoPassengerWork * MODE_AutoPassenger + BMODE_BikeWalkOtherWork * MODE_BikeWalkOther  +  BEduc_HSOrLessOtherWork * Educ_HSOrLessOther  +\
     BFuelCostWork * FuelCost + BFareCOSTWork * FareCOST  + BTCIMPWork * TCIMP
     
B_Income_RND = B_Income + sigma_income * bioDraws('B_Income_RND', 'NORMAL') + BWdaysIncome * WDAYSs + \
    BPrimarySourceIncomeIncome  * PrimarySourceIncome + BCommute_NoIncome * Commute_No +  BHHINC_Less50Income  * HHINC_Less50 + \
    BHHINC_150PlusIncome * HHINC_150Plus + BHHINC_MissingIncome * HHINC_Missing  +  BMODE_AutoPassengerIncome * MODE_AutoPassenger + \
    BEduc_MasterIncome * Educ_Master + BLivToWork_RuralIncome * LivToWork_Rural +BWTimeImpIncome * WTimeImp + BWorkSatIncome * WorkSat + \
    BLivToWork_SubUrbIncome * LivToWork_SubUrb + BComOrWork_WorkIncome * ComOrWork_Work  + BTTImpIncome * TTImp

B_Cost_RND = B_Cost + sigma_cost * bioDraws('B_Cost_RND', 'NORMAL') + BAgeCost *Ages + \
    BPrimarySourceIncomeCost  * PrimarySourceIncome + BCommute_NoCost * Commute_No +  BCDaysCost * CDayss +\
    BHHINC_Less50Cost  * HHINC_Less50  + BGend_MaleCost * Gend_Male +  BMODE_BikeWalkOtherCost * MODE_BikeWalkOther  + \
    BEduc_MasterCost * Educ_Master + BParkingCOstCost * ParkingCOst + BTollCostCost * TollCost + \
    BLivToWork_SubCost * LivToWork_Sub + BComOrWork_WorkCost * ComOrWork_Work + BIdealComTime_1To5Cost * IdealComTime_1To5 +\
    BIdealComTime_ZeroCost * IdealComTime_Zero  + BIncImpCost * IncImp + BTCIMPCost * TCIMP
   

B_Time_RND = B_Time + sigma_time * bioDraws('B_Time_RND', 'NORMAL') + BFlexWhrsTime * FlexWhrs + BRaceTime * RaceW +\
     BCommute_NoTime * Commute_No +  BHHINC_100To150Time * HHINC_100To150 + BMODE_AutoPassengerTime * MODE_AutoPassenger + \
     BMODE_BikeWalkOtherTime * MODE_BikeWalkOther  + BLivingPlace_UrbanTime * LivingPlace_Urban  +  BComOrWork_WorkTime * ComOrWork_Work +\
     BWTimeImpTime * WTimeImp 



V1 = ASC_Current + B_Time_RND * TT_C + B_Cost_RND * TC_C + B_Work_RND * WT_C + B_Income_RND * In_C
V2 = B_Time_RND * TT_A + B_Cost_RND * TC_A + B_Work_RND * WT_A + B_Income_RND * IN_A
V3 = B_Time_RND * TT_B + B_Cost_RND * TC_B + B_Work_RND * WT_B + B_Income_RND * IN_B

V = {1 : V1,
     2 : V2,
     3 : V3} 

#### Finally availability same for all
av = {1 : av,
      2 : av,
      3 : av}

### Now add eight estimation at a time

### One ####

## Proceeding towards estimation
obsprob = models.logit(V,av,choice) 

## Panel nature of the data
condprobIndiv = PanelLikelihoodTrajectory(obsprob)

logprob = log(MonteCarlo(condprobIndiv))

import biogeme.messaging as msg
logger = msg.bioMessage()
#logger.setSilent()
#logger.setWarning()
#logger.setGeneral()
logger.setDetailed()

biogeme = bio.BIOGEME(database,logprob,numberOfDraws = 10,suggestScales=False)
biogeme.loadSavedIteration()
biogeme.modelName = "10allHetro"
results = biogeme.estimate(saveIterations=True)
pandasResults = results.getEstimatedParameters()
print(pandasResults) 


