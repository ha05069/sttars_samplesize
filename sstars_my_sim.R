# Steve Harris
# 2020-06-02
# Hand crafted attempt to get a handle on sample size for the nudge trial

# Libraries
# install.packages('AER', 'ivpack', 'cowplot')
library(AER)
# library(ivpack)
library(data.table)
library(janitor)
library(ggplot2)
library(cowplot)

# Let's make an example patient; eventually this patient will be a 'row' in a data frame we'll feed into our IV equation
dt <- data.table(id=1,          # unique patient id
                 dead=1,        # survival status 1=dead
                 cpap_first=1,  # treatment status (early cpap) 1=TRUE
                 nudge=1)       # nudge (instrument) 1=TRUE


m <- ivreg(dead ~ cpap_first | nudge, data=dt)
summary(m)
# So if that's the target then how might these variables be related
N <- 10 # Number of patients


# then we generate our nudges: nudging to treatment (1=nudged to treat with CPAP first)
sample(c(0,1), N, replace=TRUE, prob=c(0.5,0.5))
cpap_nudge <- function(N) {
  return(sample(c(0,1), N, replace=TRUE, prob=c(0.5,0.5)))
}
nudges_ <- cpap_nudge(N)
nudges_

# then think about how the cpap first variable is generated
p_cpap_baseline <- 0.4 # prob cpap first at baseline
p_cpap_nudge <- 0.6 # prob cpap first if nudged
rbinom(n=N, size=1, prob=c(p_cpap_baseline, p_cpap_nudge))
cpap_first <- function(nudge, p_cpap_baseline , p_cpap_nudge ){
  p <- ifelse(nudge, p_cpap_nudge, p_cpap_baseline)
  return(rbinom(length(p), size=1, prob=p))
}
cpap_first_ <- cpap_first(nudges_, 0, 1)
cpap_first_

p_dead_baseline     <- 0.6 # 60% mortality baseline
p_dead_cpap         <- 0.5 # 50% mortality with cpap first
# survivor status: rbinom(n=N, size=1, prob=c(p_cpap_first,p_early_imv) )
# where size refers to the number of bernoulli trials (1 per patient)
rbinom(n=N, size=1, prob=c(p_dead_baseline,p_dead_cpap) )
dead <- function(cpap_first, p_dead_baseline , p_dead_cpap ){
  p <- ifelse(cpap_first, p_dead_cpap, p_dead_baseline)
  return(rbinom(n=length(p), size=1, prob=p))
}
deaths_ <- dead(cpap_first_, 1, 0)
deaths_



# Put it all together
N <- 10 # Number of patients
dt <- data.table(id=1:N)
dt[, nudge_cpap := nudge(.N)]
dt[, cpap_first := cpap_first(nudge_cpap, p_cpap_baseline=0.2, p_cpap_nudge=0.8)]
dt[, dead := dead(cpap_first, p_dead_baseline=1, p_dead_cpap=0)]
head(dt)
dt


gen_trial <- function(N,
                      p_cpap_baseline, p_cpap_nudge,
                      p_dead_baseline, p_dead_cpap){
  dt <- data.table(id=1:N)
  dt[, nudge_cpap := nudge(.N)]
  dt[, cpap_first := cpap_first(nudge_cpap, p_cpap_baseline=p_cpap_baseline, p_cpap_nudge=p_cpap_nudge)]
  dt[, complier := ifelse(nudge_cpap == cpap_first,TRUE, FALSE)]
  dt[, dead := dead(cpap_first, p_dead_baseline = p_dead_baseline, p_dead_cpap = p_dead_cpap)]
  return(dt)
}

# Check with a single trial; seem to recover the baseline and treated mortality (perfect nudge)
N <- 1e4
dt <- gen_trial(N, 
                p_cpap_baseline = 0, p_cpap_nudge = 1,
                p_dead_baseline = 0.6, p_dead_cpap = 0.4)
head(dt)
m <- ivreg(dead ~ cpap_first | nudge_cpap, data=dt)
summary(m)

# Check with a single trial and imperfect compliance
N <- 1e4
dt <- gen_trial(N, 
                p_cpap_baseline = 0.3, p_cpap_nudge = 0.7,
                p_dead_baseline = 0.6, p_dead_cpap = 0.4)
head(dt)
tabyl(dt$complier)
m <- ivreg(dead ~ cpap_first | nudge_cpap, data=dt)
res <- summary(m)$coefficients[2,]
res 
res$N <- N
unlist(res)

?try
# Now evaluate a range of sample sizes
ivreg_results <- function(N, 
                  p_cpap_baseline, p_cpap_nudge,
                  p_dead_baseline, p_dead_cpap){
                          
  dt <- gen_trial(N, 
                  p_cpap_baseline = p_cpap_baseline, p_cpap_nudge = p_cpap_nudge,
                  p_dead_baseline = p_dead_baseline, p_dead_cpap = p_dead_cpap)
  m <- tryCatch(
    expr = {
      ivreg(dead ~ cpap_first | nudge_cpap, data=dt)
      m <- ivreg(dead ~ cpap_first | nudge_cpap, data=dt)
      res <- summary(m)$coefficients[2,]
      suppressWarnings(res$N <- N)
      suppressWarnings(res$compliance <- mean(dt$complier))
      return(res)
      },
    error = function(e) {message(e)}
    
  )
}

# test this
ivreg_results(1e3,
              p_cpap_baseline = 0.3, p_cpap_nudge = 0.7,
              p_dead_baseline = 0.6, p_dead_cpap = 0.4)

# test combining into a data.table
Ns <- list(1e2,1e3,1e4)
l <- lapply(Ns, ivreg_results,
              p_cpap_baseline = 0.3, p_cpap_nudge = 0.7,
              p_dead_baseline = 0.6, p_dead_cpap = 0.4)
rbindlist(lapply(l, stack), idcol = TRUE)

# Now evaluate over a range of sample sizes
Ns <- seq(10,1000,10)
l <- lapply(Ns, ivreg_results,
              p_cpap_baseline = 0.4, p_cpap_nudge = 0.6,
              p_dead_baseline = 0.55, p_dead_cpap = 0.45)
dt <- rbindlist(lapply(l, stack), idcol = TRUE)
dt <- dcast.data.table(dt, .id ~ ind, value.var = 'values')

ggplot(dt, aes(x=N, y=Estimate)) + 
  geom_point(size=1/1, alpha=1/5) + geom_smooth() +
  geom_hline(yintercept=0) + 
  coord_cartesian(ylim=c(-0.5,+0.5)) +
  theme_cowplot()

# Now try varying the nudge compliance
Ns <- seq(10,100,10)
nudges <- seq(0.4,0.6,0.02)

gen_trial_by_nudge <- function(Ns, nudges, p_cpap_baseline=0.5){
  for (i in 1:length(nudges)) {
    p_cpap_nudge <- p_cpap_baseline + nudges[i]
    l <- lapply(Ns, ivreg_results,
                  p_cpap_baseline = p_cpap_baseline, p_cpap_nudge = p_cpap_nudge,
                  p_dead_baseline = 0.55, p_dead_cpap = 0.45)
    tryCatch(
      expr = {
        dt <- rbindlist(lapply(l, stack), idcol = TRUE)
        dt$p_nudge_delta <- nudges[i]
        if (i==1) {
          res <- dt
        } else {
          res <- rbindlist(list(res, dt),fill=TRUE)
        }
      },
      error = function(e) {message(e)}
    )
  }
  return(res)
    
}

Ns <- seq(50,4000,50)
nudges <- seq(-0.0,+0.30,0.02)
dt <- gen_trial_by_nudge(Ns, nudges)
dt <- dcast.data.table(dt, .id + p_nudge_delta ~ ind, value.var = 'values')

ggplot(dt, aes(x=(N), y=Estimate)) + 
  geom_point(size=1/1, alpha=1/5) + geom_smooth(span=10) +
  geom_hline(yintercept=0) + 
  coord_cartesian(ylim=c(-1.0,+1.0)) +
  facet_wrap(~as.factor(p_nudge_delta), ncol=4) +
  theme_cowplot()
