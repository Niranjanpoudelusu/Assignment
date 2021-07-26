
### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(dplyr)
library(readr)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="Mixed logit cost and income are lognoraml",
  modelDescr ="Two ascs, sc adressed ",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 8
)

database = read_csv("Data.csv")
database %>%
  mutate(ZeroCost = ifelse(TC_C == 0, 1, 0)) -> database
database <- as.data.frame(database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta = c(m_asc1 = 0.95718,
                sigma_asc1 = 1.34234,
                m_time = -0.07735,
                sigma_time = -0.07023,
                m_cost = -0.42251,
                sigma_cost = 0.39913,
                m_inc = -2.74790,
                sigma_inc = 1.96309,
                m_work = -0.02540,
                sigma_work = 0.02540,
                m_asc2 = 0.29819,
                sigma_asc2 = -0.6214)

### Fixed parameters 
apollo_fixed =c()

## Setting parameters for generating draws

apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws = 1000,
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
  randcoeff[["time"]] =  m_time  + sigma_time * draws_time 
  randcoeff[["work"]] = m_work  + sigma_work * draws_work
  randcoeff[["inc"]] =  exp(m_inc  + sigma_inc * draws_inc) 
  randcoeff[["cost"]] = -exp( m_cost  + sigma_cost * draws_cost) * (1-ZeroCost) 
  
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

conditionals <- apollo_conditionals(model,apollo_probabilities, apollo_inputs)

### Next we make the dataframe of individual VTTS values

Cond <- data.frame(matrix(, nrow = 611, ncol = 0))

Cond$Id <- conditionals$time[, 1]

Cond %>% mutate(time = conditionals$time[, 2],
                work = conditionals$work[, 2],
                income = conditionals$inc[, 2]) -> Cond

## Figuring out the conditionals for zero cost

### First saving the conditions
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

Indlog <- Ind
save(Indlog, file = "Indlog.Rdata")
