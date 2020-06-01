# Steve Harris
# 2020-05-04
library(tidyverse)
library(data.table)

# What is the duration of exposure to CPAP

coin_prob <- 0.20
patients <- 1e2
shifts <- 9 # 5 days when assume that all patients remain on CPAP at the final point
simulations <- 1e3

cpap_duration <- function(patients, shifts, coin_prob) {
  # simulate the coin toss
  dd <- t(replicate(patients, rbinom(shifts,1,coin_prob)))
  # identify the first time the coin is '1' (i.e. IMV)
  exposure <- apply(dd, 1, match, x=1)
  return(exposure)
}


# single trial
exposure <- cpap_duration(patients, shifts, coin_prob = coin_prob )
df <- data.frame(patients,exposure)
ggplot(data=df, aes(x=exposure)) + geom_histogram()

# multiple trials
dt <- data.table(t(replicate(simulations, 
          cpap_duration(patients, shifts, coin_prob = coin_prob))))
dt[, sim :=  1:simulations]
dt_long <- melt.data.table(dt, id.vars = "sim")

ggplot(dt_long, aes(x=value, after_stat(count), group=sim)) + 
  geom_density(colour="#00000011", 
               alpha=1/1e3, # forces alphas for a line
               na.rm=TRUE,  # include rows with missing values in density
               adjust=1.2   # adjust bandwidth to smooth better
               ) + 
  ylab("Number of patients within a 100 patient trial") +
  xlab("Number of shifts exposed to CPAP\n(Multiply by 12 to get hours exposed)") +
  scale_x_continuous(limits=c(1,10), breaks=1:10, minor_breaks = NULL) +
  ggtitle("Simulated distributions of CPAP exposure over 5 days (10 shifts)") +
  theme_minimal()

# median number of shifts (or CPAP exposure)
summary(apply(dt, 1, median, na.rm=TRUE))

summary(apply(dt, 1, quantile, probs=c(0.25), na.rm=TRUE))
summary(apply(dt, 1, quantile, probs=c(0.75), na.rm=TRUE))

