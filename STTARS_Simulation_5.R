# Aims:
# Define participant flow through the study following the initial randomisation
# Create a simulated data set to represent this study flow

# we have a sample of 1000 patients enrolled.

n <- c(1:1000)

tbl <- tibble(n)

# at this point, each patient has received their initial randomisation allocation.

# all these 1000 patients have been allocated CPAP as we are concerned only with the sequential randomisation
# in this script.

# at the first review point of 12 hours, a sequence of events will occur.

# first, all 1000 patients are screened to see if they are eligible for re-randomisation
# they would not be eligible if they were ventilated at the time of review for example.
# eligibility is determined by examining the patient state at 12 hours, defined as X_at_12, where 1 == ventilated
# and 0 == cpap.

  # allocate a random 1 or 0 to each patient:

tbl <- 
  tbl %>% 
  mutate(x_at_12 = sample(0:1, size = 1000, replace = T))

  # create a variable describing their eligibility for randomisation based on x_at_12:

tbl <- 
  tbl %>% 
  mutate(elig_for_R = if_else(x_at_12 == 1, FALSE, TRUE))

# after determining eligibility for re-randomisation, the clinician receives the nudge prompt to consider 
# re-randomisation:

# "Do you have equipoise to submit this patient for randomisation?"

# the clinician can either say YES or NO at this point.

# start with the assumption that in 10% of cases the clinician will have enough equipoise to submit 
# the patient to re-randomisation.

  # select the eligible patients into a new object:

elig_sample <- 
  tbl %>% 
  filter(elig_for_R == T)

  # select a random 10% sample of the eligible patients who the clinician has equipoise for:

equipoise <- 
  elig_sample %>% 
  sample_frac(0.10)

  # create a new column for the selected, eligible patients in the main table:

tbl <- 
  tbl %>% 
  mutate(equipoise_at_12 = if_else(tbl$n %in% equipoise$n, T, F))

# next the eligible patients with equipoise are submitted to re-randomisation 1:1:

tbl <- 
  tbl %>% 
  mutate(r_at_12 = NA) %>% 
  mutate(r_at_12 = ifelse(elig_for_R == T & equipoise_at_12 == T, T, NA)) %>% 
  mutate(r_at_12 = ifelse(r_at_12 == T, sample(0:1,
                                               size = sum(!is.na(r_at_12)),
                                               replace = T)))

# the next decision is, for those eligible patients, with equipoise, 
# does the clinician then comply with the unmasked allocation recommendation:

# let's assume a 10% rate of non-compliance with the randomised allocation
# you would hope that this would be a low rate given the clinician has already made the decision to randomise

  # select a random 10% sample of patients who have been randomised and label them as non-compliers:

randomised <- 
  tbl %>% 
  filter(!is.na(r_at_12))

non_comp <- 
  randomised %>% 
  sample_frac(0.10)

tbl <- 
  tbl %>% 
  mutate(non_compliance = if_else(tbl$n %in% non_comp$n, T, F))

# based on:
  # eligibility for randomisation (not ventilated) +
  # clinician equipoise for randomisation (10%) +
  # 1:1 re-randomisation +
  # non-compliance with randomisation (10%)

# the patient now moves through the next 12 hours of observation to the next review point at 24 hours.

# next generate the observed patient state at 24 hours (x_at_24).
# this is a combination of:
  # 1. if they were randomised at 12 hours +
  # 2. if the clinician complied with the allocation at 12 hours +
  # 3. if they deteriorated to IMV during the observation period

# for this example assume a 5% rate of unintended cross over (composed of deterioration to IMV or 
# improvement to facemask oxygen)

tbl <- 
  tbl %>% 
  mutate(x_at_24 = NA) %>% 
  mutate(x_at_24 = ifelse(x_at_12 == 1, 1, NA)) %>% 
  mutate(x_at_24 = ifelse(elig_for_R == T & equipoise_at_12 == T & non_compliance == F, 
                          r_at_12, 
                          r_at_12 + 1))
  # here if the patient was eligible and randomised but the clinician did not comply then x_at_24 will either be
  # 1 (if randomised to cpap and then intubated) or 2 (if randomised to IMV but then cpap'd)

  # then revert the 2s back to 0s:

tbl <- 
  tbl %>% 
  mutate(x_at_24 = if_else(x_at_24 == 2, 0, x_at_24))

# now add in a random selection of patients who crossed over to either IMV or facemask O2 (5%):

  # filter all the people who could have moved group:
  # these are all the people who were randomised to cpap and where the clinician complied
  # in this example, where x_at_24 == 0

crossover_potentials <- 
  tbl %>% 
  filter(x_at_24 == 0)

crossover <- 
  crossover_potentials %>% 
  sample_frac(0.05)

tbl <- 
  tbl %>% 
  mutate(x_at_24 = if_else(tbl$n %in% crossover$n, 1, x_at_24)) # (note this does not differentiate crossover between
# escalated to IMV or downgraded to O, merely that they are not on CPAP anymore and as such 
# should not progress to the next round of randomisation)

# the simulation can then continue under the same assumptions at each timepoint until there are 
# no patients left on cpap


# ============================================================================================ #

# make a graph showing the breakdown of study participants at each review point:

tbl %>% 
  ggplot(aes(x = as.factor(x_at_12))) +
  geom_bar()

tbl %>%
  filter(!is.na(x_at_24)) %>% 
  ggplot(aes(x = as.factor(x_at_24))) +
  geom_bar()

# ============================================================================================ #

# ideally this would be automated/condensed into some kind of function which would take 
# sample size (number), 
# an initial proportion eligible (x_at_12 != IMV) (%),
# an estimate of the proportion of the eligible patients with equipoise (%),
# an estimate of the proportion of non-compliance following re-randomisation (%),
# an estimate of the proportion of patients who deteriorated to IMV, improved to O or died (%)

# it would give as outputs:
# number of patients continuing in the trial at each time point

n <- 1000
prop.eligible <- 0.5
prop.equipoise <- 0.1
prop.noncomp <- 0.1
prop.crossover <- 0.05

simulate <- 
  function(
    n,
    prop.eligible,
    prop.equipoise,
    prop.noncomp,
    prop.crossover) {
    
    
  }
