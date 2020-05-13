# experimenting with basic sample size calculations for a binary outcome using {stats} package

library(tidyverse)

# Aim is to generate a heatmap illustrating different sample sizes, first with a fixed alpha and beta but varying by P1 and P2
# and then by fixing P1 and P2 and varying the alpha and beta.


# Definitions:
  # Alpha: significance level - i.e. the strength of the evidence we require to reject the null hypothesis
  # Beta: the probability of obtaining the stated level of significance, the power of the study (1 - Beta)
  # P1: the treatment effect in group 1, the probability of the outcome occurring in group 1 [IMV]
  # P2: the treatment effect in group 2, the probability of the outcome occurring in group 2 [CPAP]
  # Absolute risk: I am taking this to mean the risk difference i.e. the number of people who die in group 1 minus the number of people who die in group 2

# Context:

# We want to study whether one type of ventilation (CPAP) is better than another (IMV) at reducing mortality at 14 days (binary outcome)
# How many patients would we need to study to conclude with a reasonable degree of certainty that CPAP is superior to IMV?
# We might define superiority in this context as a reduction in absolute risk of death at 14 days by 0.1 (10%), this being an estimated clinically significant endpoint which can be subjected to further testing across a range of values.

# Let's start by stating that we would like a 90% power to detect a significant result at the 5% level.  
# The null hypothesis is that there is no difference in the proportion of people who have died at 14 days in each treatment group.


# 1.  Fixed alpha of 5% and fixed beta of 90%, with varying absolute risk reductions:

# 10%

power.prop.test(n = NULL, # because this is the quantity we want to estimate
                p1 = 0.5, # 50% probability of death at 14 days in the CPAP group
                p2 = 0.6, # 60% probability of death at 14 days in the IMV group, i.e. a 10% absolute risk reduction in the cpap group
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided") # with default values for 'strict' and 'tol'

  # gives n = 519 when rounded to a whole person.  When I put the same values into the sealedenvelope.com calculator, it
  # comes out with 516 per group...?

# 5%

power.prop.test(n = NULL, 
                p1 = 0.55, # cpap
                p2 = 0.6,  # imv
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided")

# 1%


power.prop.test(n = NULL, 
                p1 = 0.59, # cpap
                p2 = 0.6,  # imv
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided")


# -1% i.e. an increase in 14 day mortality in the cpap group


power.prop.test(n = NULL, 
                p1 = 0.61, # cpap
                p2 = 0.60,  # imv
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided")

# -5% 


power.prop.test(n = NULL, 
                p1 = 0.65, # cpap
                p2 = 0.6,  # imv
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided")

# -10%


power.prop.test(n = NULL, 
                p1 = 0.7, # cpap
                p2 = 0.6,  # imv
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided")

# Test: does the actual probability values you put into p1 and p2 make a difference or is it just the proportional difference between them?

# 5% proportion with smaller p1 and p2 values:

power.prop.test(n = NULL, 
                p1 = 0.05, # cpap
                p2 = 0.1,  # imv
                sig.level = 0.05,
                power = 0.9,
                alternative = "two.sided")
# n = 582 i.e. not far off the 519 we got for the same proportional difference at higher p1 and p2 values.







# Results Table

sample <- 
  tibble(
    "Alpha" = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05),
    "Beta" = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9),
    "p1" = c(0.5, 0.55, 0.59, 0.61, 0.65, 0.7),
    "p2" = c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6),
    "ARR" = c(0.1, 0.05, 0.01, -0.01, -0.05, -0.1),
    "n/group" = c(519, 2053, 50639, 50219, 1969, 476)
  )


# Graphs

p2 <- c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6)
p1 <- c(0.5, 0.55, 0.59, 0.61, 0.65, 0.7)
sample_size <- c(519, 2053, 50639, 50219, 1969, 476)
arr <- c(0.1, 0.05, 0.01, -0.01, -0.05, -0.1)

sample %>% 
  ggplot(aes(x = arr, y = sample_size)) +
  geom_point() +
  geom_smooth()
