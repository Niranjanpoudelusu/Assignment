
### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(readr)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="Preference heterogenity model",
  modelDescr ="Two ascs, sc adressed ",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 8
)

database = read_csv("Data.csv")
database <- as.data.frame(database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta = c(m_asc1 = 0.938,
                sigma_asc1 = 1.38,
                m_time = -0.0694,
                sigma_time = 0.0712,
                m_cost = -0.559,
                sigma_cost = 0.404,
                m_inc = 0.253,
                sigma_inc = 0.204,
                m_work = -0.0179,
                sigma_work = -0.0282,
                m_asc2 = 0.278,
                sigma_asc2 = -0.642,
                age_c = -0.00621,
                cdays_c = 0.0464,
                cdays_i = -0.0147,
                commute_no_c = -0.0799,
                commute_no_i = 0.0682,
                commute_no_t = -0.0412,
                commute_no_w = -0.0176,
                educ_master_i = -0.0331,
                flexwhrs_t = 0.0181,
                hhinc_100to150_t = 0.0226,
                hhinc_less50_i = 0.147,
                hhinc_missing_i =0.454,
                idealcomtime_15_i = -0.0778,
                idealcomtime_zero_c = -0.211,
                livtowork_rural_i = 0.212,
                livtowork_suburb_i = 0.0662,
                livingplace_urban_t = 0.0224,
                mode_autopassenger_t =0.0429,
                mode_autopassenger_w =0.044,
                mode_bikewalkother_i = 0.177,
                mode_bikewalkother_t = 0.044,
                parkingcost_c = 0.178,
                primarysourceincome_c = 0.193,
                primarysourceincome_i = -0.0984,
                race_t = -0.0331,
                tollcost_c = 0.152
                )

### Fixed parameters 
apollo_fixed =c()

## Setting parameters for generating draws

apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws = 3000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc1","draws_asc2","draws_time","draws_cost","draws_work","draws_inc"),
  intraDrawsType = "pmc",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
  
)

### Creating the random parameters 
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["asc1"]] =  m_asc1  + sigma_asc1 * draws_asc1 
  randcoeff[["asc2"]] =  m_asc2  + sigma_asc2 * draws_asc2 
  randcoeff[["time"]] =  m_time  + sigma_time * draws_time + commute_no_t*Commute_No + flexwhrs_t * FlexWhrs + 
    hhinc_100to150_t * HHINC_100To150 + livingplace_urban_t * LivingPlace_Urban + mode_autopassenger_t * MODE_AutoPassenger +
    mode_bikewalkother_t * MODE_BikeWalkOther + race_t * RaceW
  
  randcoeff[["work"]] =  m_work  + sigma_work * draws_work + commute_no_w * Commute_No + mode_autopassenger_w *MODE_AutoPassenger
  
  randcoeff[["inc"]] =  m_inc  + sigma_inc * draws_inc + cdays_i * CDays + commute_no_i * Commute_No +
    educ_master_i * Educ_Master + hhinc_less50_i * HHINC_Less50 + hhinc_missing_i * HHINC_Missing +
    idealcomtime_15_i * IdealComTime_15Plus + livtowork_rural_i * LivToWork_Rural +
    livtowork_suburb_i * LivToWork_SubUrb + mode_bikewalkother_i * MODE_BikeWalkOther +
    primarysourceincome_i * PrimarySourceIncome
    
  
  randcoeff[["cost"]] =  m_cost  + sigma_cost * draws_cost + age_c *Age + cdays_c * CDays + commute_no_c *Commute_No +
    idealcomtime_zero_c * IdealComTime_Zero + parkingcost_c * ParkingCOst + primarysourceincome_c * PrimarySourceIncome +
    tollcost_c * TollCost
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Utilities
  V = list()
  V[["Current"]] = asc1 + time * TT_C + work * WT_C + cost * TC_C + inc * In_C
  V[["A"]] = asc2 + time * TT_A + work * WT_A + cost * TC_A + inc * IN_A
  V[["B"]] = time * TT_B + work * WT_B + cost * TC_B + inc * IN_B
  
  ### Defining settings for the MNL model component
  mnl_settings = list(
    alternatives = c(Current =1, A =2, B =3),
    avail = list(Current = 1, A =1, B =1),
    choiceVar = choice,
    V = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))


apollo_modelOutput(model, modelOutput_settings = list(printPVal = 2)) 


apollo_saveOutput(model, saveOutput_settings = list(printPVal = 2))

conditionals1 <- apollo_conditionals(model,apollo_probabilities, apollo_inputs)



Cond <- data.frame(matrix(, nrow = 611, ncol = 0))

Cond$Id <- conditionals$time[, 1]

Cond %>% mutate(time = conditionals$time[, 2],
                work = conditionals$work[, 2],
                income = conditionals$inc[, 2]) -> Cond

#save(conditionals, file = "conditionals.Rdata")


database %>% filter(TC_C > 0) -> database

apollo_inputs = apollo_validateInputs()

newcond <- apollo_conditionals(model, apollo_probabilities, apollo_inputs)
newcost <- as.data.frame(newcond$cost[, c(1,2)])

#save(newcond, file = "zerocond.Rdata")

### Combining the conditional estimates
Ind <- merge(Cond, newcost, by.x = "Id", by.y = "ID", all.x = T)

names(Ind) <- c("ID", "time", "work", "inc", "cost")

#save(Ind, file = "Ind.Rdata")

### Next we plot the parameter estimates 
par(mfrow = c(2,2))

plot(density(Ind$time, na.rm = T), col ="red", lwd = 2,
     main = "Distribution for Travel time parameters")
# lines(density(Ind1$time, na.rm = T), col ="blue", lwd = 2)
# legend (x = "topleft", legend = c("Income", "Net Income"),
#         col = c("red", "blue"), lty=1:1, cex=0.8)

plot(density(Ind$work, na.rm = T), col ="red", lwd = 2,
     main = "Distribution for Work time parameters")
# lines(density(Ind1$work, na.rm = T), col ="blue", lwd = 2)
# legend (x = "topleft", legend = c("Income", "Net Income"),
#         col = c("red", "blue"), lty=1:1, cex=0.8)

plot(density(Ind$cost, na.rm = T), col ="red", lwd = 2, 
     main = "Distribution for Travel cost parameters")
# lines(density(Ind1$cost, na.rm = T), col ="blue", lwd = 2)
# legend (x = "topleft", legend = c("Income", "Net Income"),
#         col = c("red", "blue"), lty=1:1, cex=0.8)

plot(density(Ind$inc, na.rm = T), col ="red", lwd = 2, 
     main = "Distribution for Income parameters")
# lines(density(Ind1$Net, na.rm = T), col ="blue", lwd = 2)
# legend (x = "topleft", legend = c("Income", "Net Income"),
#         col = c("red", "blue"), lty=1:1, cex=0.8)

plot(density(Ind$cost, na.rm = T), col ="red", lwd = 2, 
     main = "Zoomed in distribution of Travel cost", xlim = c(-3,1))

plot(density(Ind$inc, na.rm = T), col ="red", lwd = 2, 
     main = "Zoomed in distribution of Income", xlim = c(0,2))

### Next we calculate the willingness to pay measures for the data

Indheterogenity <- Ind
save(Indheterogenity, file = "Indhetro.Rdata")


a <- Ind$time/Ind$inc
plot(density(a), xlim =c(-200, 200))

mean(a)
