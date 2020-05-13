# Premise:

# We are interested in testing the effect that continuing CPAP has on 14 day mortality
# vs. early IMV in COVID 19 ARF.

# We design an RCT in which people needing CPAP are initially randomised to early IMV
# or to continue CPAP in stage 1. 

# In stage 2 we attempt to use multiple nudge randomisation events to provide a structure
# to attempt to define the optimal time period for CPAP prior to IMV. 

# Given there is no choice between IMV and CPAP in this population, this will be a 
# comparative effectiveness study and won't specifically have a control group.



# Aim:

# Simulate stage 1 of the trial, one randomisation event following recruitment, with
# inevitable non-compliance with the randomisation recommendation and subsequent, 
# unintentional cross over from the continued CPAP group into the IMV group.


# Q. Do the patients who get IMV following randomisation to CPAP just get analysed
# in the CPAP group based on ITT and we adjust for the duration of CPAP treatment
# in the analysis?


# define a function that simulates a population and estimates results:

sttarIV <- function(
  nsim = 100,
  npop = 1000, # define the sampling population
  cpap = 0.5,  # define the proportion of the sample population randomised to cpap
  imv = 0.5,   # define the proportion of the sample population randomised to imv
  cpap_comp = 0.8,  # est. cpap usage amongst the cpap group (i.e. compliance with randomisation)
  imv_comp = 0.8,   # est. imv usage amongst the imv group (i.e. compliance)
  cpap_mort = 0.6,  # est. mortality in cpap group
  imv_mort = 0.8,   # est. mortality in imv group (i.e. 20% higher)
  alpha = .05
) {
  # define a vector to store results
  pvalues <- NULL
  
  for (i in 1:nsim) { # repeat the simulation a number of times
    # generate the sample population:
    simdata <- data.frame(
      cpap_list = c(rep(1, npop*cpap), rep(0, npop*imv)),
      imv_list = c(rep(0, npop*cpap), rep(1, npop*imv)))
      
      # calculate the actual population generated:
      
      npeople <- nrow(simdata)
      
      # generate the rate of randomisation compliance:
      
      simdata$comp <- 
        simdata$cpap *rbinom(npeople, 1, cpap_comp) +
        simdata$imv *rbinom(npeople, 1, imv_comp)
      
      # generate outcome rates:
      
      simdata$outcome <- 
        (simdata$comp == "cpap")*rbinom(npeople, 1, cpap_mort) +
        (simdata$comp == "imv")*rbinom(npeople, 1, imv_mort)
      
      # the example includes an estimator for TP and FP for lab tests but doesn't apply to detection of our outcome
      # they then use this as an instrument for the later stage IV.
      
      # simple estimation of the effect of treatment on mortality
      
      glmcoef <- 
        summary(glm(outcome ~ cpap_list + imv_list, data = simdata))$coefficients
      
      # 2SLS to estimate the effect of cpap or imv on mortality
      
      # leave out for now, until identify a suitable instrument
      
      # ivregest <- ivreg(INSTR ~ outcome | cpap + imv, data = simdata)
      # ivregcoef <- summary(ivregest)$coefficients
      
      # save the rejection of the null rates
      
      pvalues <- rbind(pvalues, c(cpap = glmcoef[1,4],
                                  imv = glmcoef[2,4]))
                                  #iv = ivregcoef[2,4]))
      
      
  }
  # calculate mean rejection rate for each coefficient
  apply(pvalues, 2, mean)
}

sttarIV()  






