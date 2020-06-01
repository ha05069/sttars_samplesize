library(tidyverse)

# generate empty matrix:



n_patients <- 100

dt <- as.tibble(data.frame(
  pt_id = 1:n_patients,
  # make all patients on cpap at x = 0:
  x_at_0 = 1,
  # o2 status at 12
  x_at_12 = sample(0:2, 
           # 0 = cpap
           # 1 = IMV
           # 2 = simple O2
           # prob = c(0.9, 0.25, 0.25), # <--- prob must add to 1 
           prob = c(0.5, 0.25, 0.25), 
           size = n_patients, 
           replace = T),
# eligible for randomisation if on cpap at x_at_12
  elig_at_12 = if_else(dt$x_at_12 == 0, T, F)
  # R_at_12 = "V5",
  # x_at_24 = "V6",
  # elig_at_24 = "V7",
  # R_at_24 = "V8",
  # x_at_36 = "V9",
  # elig_at_36 = "V10",
  # R_at_36 = "V11",
  # x_at_48 = "V12"
))
dt





# x_at_24 is determined by:
# 1. Does the nudge encourage action, or change the patient's current state i.e. nudge to IMV?
# 2. Does the clinician comply with the recommended nudge?
# 3. Does the patient state change irrespective of the nudge?
# 3. a. CPAP -> IMV (deterioration)
# 3. b. CPAP -> O (improvement)

# apply a series of assumptions:

# 1. That during the next 12 hour period, 10% of patients will deteriorate and require IMV
# 2. That 10% will improve and move to O
# 3. That clinicians will comply with the direction of the nudge 50% of the time (unobserved)

# first generate the patient state at 24 hours based on what would have happened if the clinician 
# had not intervened.  Then change the results to incorporate the proportion which would have had their
# state changed by the clinician following the nudge.

# Note - the timings are difficult here because the patient may change state independently before the
# clinician has an opportunity to act on the nudge.


# or maybe first consider the immediate result of the nudge which is where it requests positive action
# i.e. 0 -> 1, then the clinician must decide whether to comply or not.  This probably works better
# because the clinician has the first opportunity to apply the nudge or not and then a selection of the
# patients will deteriorate or get better following that.

# so first take all the pts where R_at_12 == 1 i.e. nudged to IMV.  
# assume 50% of these nudges will be complied with, therefore x_at_24 == 1:

dt <- 
  dt %>% 
  mutate(x_at_24 = 
           ifelse(R_at_12 == 1, 
                  sample(0:1, prob = c(0.5, 0.5), size = length(R_at_12 == 1), replace = T), 
                  NA))

# so pts eligible at x_12 have been randomised, then those randomised to 1, 50% go on to receive IMV due to the action 
# from the clinician.

# of those that were randomised to cpap AND those that were randomised to IMV but where the clinician ignored the nudge
# and they continued on cpap, a proportion will deteriorate, a proportion will improve, and the rest will stay the same 
# until the next review point.

# therefore the next step is to take the 50% where the nudge to IMV was not complied with and apply those proportions
# similarly, the same process needs to be done for the pts randomised to continue with cpap (R_at_12 == 0).

dt_1 <- 
  dt %>% 
  filter(R_at_12 == 1 & x_at_24 == 0) %>% # select the patients randomised to IMV where the clinician disregarded the nudge
  mutate(x_at_24 = sample(0:2, prob = c(0.8, 0.1, 0.1), size = length(x_at_24), replace = T)) %>%  # replace x_at_24 with a random
# outcome based on 80% likelihood of continuing cpap, 10% chance of deteriorating to IMV and 10% chance of improving to O.
  select(pt_id, x_at_24)

# join back to main table:

dt <- 
  dt %>% 
  left_join(dt_1, by = "pt_id")

# reduce down to a single x_at_24 column keeping the original 1s where the nudge was complied and replacing the non-compliers
# with the values from dt_1

dt <- 
  dt %>% 
  mutate(x_at_24.x = ifelse(R_at_12 == 1 & x_at_24.x == 0, x_at_24.y, x_at_24.x)) %>% 
  select(-x_at_24.y)

dt <- 
  dt %>% 
  rename(x_at_24 = "x_at_24.x")

# then apply the same logic to cases where patients were randomised to 0.
# the same conditions apply, either the clinician complies with nudge (continues cpap) or not (starts IMV)
# AND for the patients where the nudge is complied with the same probabilities can be applied for either deteriorating or
# improving or staying the same (10,10,80%).

# for now, keep the probability of nudge compliance the same (50%) although in reality there is likely to be a higher 
# probability of compliance with this end of the nudge as it is easier to continue CPAP and observe than it is to start IMV

# in reality, as we have discussed, this clinical decision is a continuous probability over the course of the entire shift,
# punctuated by moments where it is crystalised and expressed as decisions, on ward rounds for example.

# so take the pts randomised to 0 and apply the same 50:50 weighting to whether the nudge is complied with or not:

dt <- 
  dt %>% 
  mutate(x_at_24 = 
           ifelse(R_at_12 == 0, 
                  sample(0:1, prob = c(0.5, 0.5), size = length(R_at_12 == 0), replace = T), 
                  x_at_24))

# now take those who were randomised to 0, nudge complied with (x_at_24 currently == 0) and apply the same spectrum of 
# probabilities to "natural" change in state:

dt_1 <- 
  dt %>% 
  filter(R_at_12 == 0 & x_at_24 == 0) %>% # randomised to 0 and nudge complied with
  mutate(x_at_24 = sample(0:2, prob = c(0.8, 0.1, 0.1), size = length(x_at_24), replace = T)) %>%
  # 80% continue on CPAP
  # 10% improve to O
  # 10% deteriorate and receive IMV (even though the nudge was complied with initially)
  select(pt_id, x_at_24)

dt <- 
  dt %>% 
  left_join(dt_1, by = "pt_id")


dt <- 
  dt %>% 
  mutate(x_at_24.x = ifelse(R_at_12 == 0 & x_at_24.x == 0, x_at_24.y, x_at_24.x)) %>% 
  select(-x_at_24.y)

dt <- 
  dt %>% 
  rename(x_at_24 = "x_at_24.x")





# ======================================================================================================================= #
# ======================================================================================================================= #





# next chunk, eligibility at 24 hour randomisation point

# drop rows where eligible for randomisation at 12 hours == F i.e. patients have exited the study

dt2 <- 
  dt %>% 
  filter(elig_at_12 == T)

# eligible for randomisation if on cpap at x_at_24

dt2 <- 
  dt2 %>% 
  mutate(elig_at_24 = if_else(x_at_24 == 0, T, F))

# if eligible = T, generate random nudge for next 12 hour period (1:1)

dt2 <- 
  dt2 %>% 
  mutate(R_at_24 = 
           ifelse(elig_at_24 == T, 
                  sample(0:1, size = sum(elig_at_12 == T), replace = T), 
                  NA))
         
         

# again, assume a 50% compliance with the nudge and repeat the same processes for the IMV group first:

dt2 <- 
  dt2 %>% 
  mutate(x_at_36 = 
           ifelse(R_at_24 == 1, 
                  sample(0:1, prob = c(0.5, 0.5), size = length(R_at_24 == 1), replace = T), 
                  NA))

dt_1 <- 
  dt2 %>% 
  filter(R_at_24 == 1 & x_at_36 == 0) %>% # select the patients randomised to IMV where the clinician disregarded the nudge
  mutate(x_at_36 = sample(0:2, prob = c(0.8, 0.1, 0.1), size = length(x_at_36), replace = T)) %>%  # replace x_at_24 with a random
  # outcome based on 80% likelihood of continuing cpap, 10% chance of deteriorating to IMV and 10% chance of improving to O.
  select(pt_id, x_at_36)

dt2 <- 
  dt2 %>% 
  left_join(dt_1, by = "pt_id")

# reduce down to a single x_at_24 column keeping the original 1s where the nudge was complied and replacing the non-compliers
# with the values from dt_1

dt2 <- 
  dt2 %>% 
  mutate(x_at_36.x = ifelse(R_at_24 == 1 & x_at_36.x == 0, x_at_36.y, x_at_36.x)) %>% 
  select(-x_at_36.y)

dt2 <- 
  dt2 %>% 
  rename(x_at_36 = "x_at_36.x")

# again, take the pts randomised to 0 and apply the same 50:50 weighting to whether the nudge is complied with or not:

dt2 <- 
  dt2 %>% 
  mutate(x_at_36 = 
           ifelse(R_at_24 == 0, 
                  sample(0:1, prob = c(0.5, 0.5), size = length(R_at_24 == 0), replace = T), 
                  x_at_36))

# now take those who were randomised to 0, nudge complied with (x_at_24 currently == 0) and apply the same spectrum of 
# probabilities to "natural" change in state:

dt_1 <- 
  dt2 %>% 
  filter(R_at_24 == 0 & x_at_36 == 0) %>% # randomised to 0 and nudge complied with
  mutate(x_at_36 = sample(0:2, prob = c(0.8, 0.1, 0.1), size = length(x_at_36), replace = T)) %>%
  # 80% continue on CPAP
  # 10% improve to O
  # 10% deteriorate and receive IMV (even though the nudge was complied with initially)
  select(pt_id, x_at_36)

dt2 <- 
  dt2 %>% 
  left_join(dt_1, by = "pt_id")

dt2 <- 
  dt2 %>% 
  mutate(x_at_36.x = ifelse(R_at_24 == 0 & x_at_36.x == 0, x_at_36.y, x_at_36.x)) %>% 
  select(-x_at_36.y)

dt2 <- 
  dt2 %>% 
  rename(x_at_36 = "x_at_36.x")





# ======================================================================================================================= #
# ======================================================================================================================= #



# next chunk, eligibility at 36 hour randomisation point

# drop rows where eligible for randomisation at 24 hours == F i.e. patients have exited the study

dt3 <- 
  dt2 %>% 
  filter(elig_at_24 == T)

# eligible for randomisation if on cpap at x_at_24

dt3 <- 
  dt3 %>% 
  mutate(elig_at_36 = if_else(x_at_36 == 0, T, F))

# if eligible = T, generate random nudge for next 12 hour period (1:1)

dt3 <- 
  dt3 %>% 
  mutate(R_at_36 = 
           ifelse(elig_at_36 == T, 
                  sample(0:1, size = sum(elig_at_36 == T), replace = T), 
                  NA))



# again, assume a 50% compliance with the nudge and repeat the same processes for the IMV group first:

dt3 <- 
  dt3 %>% 
  mutate(x_at_48 = 
           ifelse(R_at_36 == 1, 
                  sample(0:1, prob = c(0.5, 0.5), size = length(R_at_36 == 1), replace = T), 
                  NA))

dt_1 <- 
  dt3 %>% 
  filter(R_at_36 == 1 & x_at_48 == 0) %>% # select the patients randomised to IMV where the clinician disregarded the nudge
  mutate(x_at_48 = sample(0:2, prob = c(0.8, 0.1, 0.1), size = length(x_at_48), replace = T)) %>%  # replace x_at_24 with a random
  # outcome based on 80% likelihood of continuing cpap, 10% chance of deteriorating to IMV and 10% chance of improving to O.
  select(pt_id, x_at_48)

dt3 <- 
  dt3 %>% 
  left_join(dt_1, by = "pt_id")

# reduce down to a single x_at_24 column keeping the original 1s where the nudge was complied and replacing the non-compliers
# with the values from dt_1

dt3 <- 
  dt3 %>% 
  mutate(x_at_48.x = ifelse(R_at_36 == 1 & x_at_48.x == 0, x_at_48.y, x_at_48.x)) %>% 
  select(-x_at_48.y)

dt3 <- 
  dt3 %>% 
  rename(x_at_48 = "x_at_48.x")

# again, take the pts randomised to 0 and apply the same 50:50 weighting to whether the nudge is complied with or not:

dt3 <- 
  dt3 %>% 
  mutate(x_at_48 = 
           ifelse(R_at_36 == 0, 
                  sample(0:1, prob = c(0.5, 0.5), size = length(R_at_36 == 0), replace = T), 
                  x_at_48))

# now take those who were randomised to 0, nudge complied with (x_at_24 currently == 0) and apply the same spectrum of 
# probabilities to "natural" change in state:

dt_1 <- 
  dt3 %>% 
  filter(R_at_36 == 0 & x_at_48 == 0) %>% # randomised to 0 and nudge complied with
  mutate(x_at_48 = sample(0:2, prob = c(0.8, 0.1, 0.1), size = length(x_at_48), replace = T)) %>%
  # 80% continue on CPAP
  # 10% improve to O
  # 10% deteriorate and receive IMV (even though the nudge was complied with initially)
  select(pt_id, x_at_48)

dt3 <- 
  dt3 %>% 
  left_join(dt_1, by = "pt_id")

dt3 <- 
  dt3 %>% 
  mutate(x_at_48.x = ifelse(R_at_36 == 0 & x_at_48.x == 0, x_at_48.y, x_at_48.x)) %>% 
  select(-x_at_48.y)

dt3 <- 
  dt3 %>% 
  rename(x_at_48 = "x_at_48.x")





# ======================================================================================================================= #
# ======================================================================================================================= #


# extract the individual pt states at each time point and combine:

x_at_n <- 
  dt %>% 
  select(pt_id, x_at_0, x_at_12, x_at_24)

dt2_x <- 
  dt2 %>% 
  select(pt_id, x_at_36)

dt3_x <- 
  dt3 %>% 
  select(pt_id, x_at_48)

x_at_n <- 
  x_at_n %>% 
  left_join(dt2_x, by = "pt_id") %>%
  left_join(dt3_x, by = "pt_id")


# add in crude outcome variable i.e. death at 14 days:

  # at this stage, do we want to try and artificially create the treatment effect we would like to see?
  # this could then be tinkered with to allow evaluation of different sample sizes?

  # try just doing a crude mortality outcome now, then create a variable for time to intubation or duration on cpap
  # then can just try a simple logistic regression at the end?

x_at_n <- 
  x_at_n %>% 
  mutate(outcome = sample(0:1, prob = c(0.6, 0.4), size = length(pt_id), replace = T)) 
# 0 == survived, 1 == died.  Start with a 40% mortality.

  # could make this more nuanced by assigning different probabilities dependent on different states
  # i.e. if you improve to 2 and exit the study your mortality is probably lower


# create cpap time variable:

x_at_n <- 
  x_at_n %>% 
  mutate(cpap_time = 
           case_when(x_at_48 == 0 ~ 48,
                     x_at_36 == 0 ~ 36,
                     x_at_24 == 0 ~ 24,
                     x_at_12 == 0 ~ 12))

x_at_n <- 
  x_at_n %>% 
  mutate(cpap_time = ifelse(is.na(cpap_time), "< 12", cpap_time))
          

x_at_n %>% 
  mutate(cpap_time = as.factor(cpap_time)) %>% 
  ggplot(aes(x = cpap_time)) +
  geom_bar()

x_at_n %>% 
  group_by(as.factor(cpap_time)) %>% 
  summarise(outcome = sum(outcome)) %>% 
  ggplot(aes(x = `as.factor(cpap_time)`, y = outcome)) +
  geom_col() +
  ylab("Number of Deaths") +
  xlab("Duration of CPAP") +
  ggtitle("Number of Deaths by Duration of CPAP Treatment")


# could maybe at this stage just do a survival analysis and stratify by duration of cpap?

# or glm?

model <- 
  glm(outcome ~ cpap_time, data = x_at_n)

summary(model)


# next steps:

# 1. Package together and automate (? how)
# 2. Define the right statistical test to apply at the end
# 3. Work out how to run multiple iterations
# 4. Simulate with different sample sizes, nudge compliance and treatment effect assumptions