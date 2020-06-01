# Steve Harris
# 2020-06-01
# Micro simulation of patients running through the trial
# All times are in hours

# Packages to install
pkgs <- c('simmer', 'simmer.plot', 'janitor', 'data.table')
install.packages(pkgs)

# rm(list=ls())
library(simmer)
library(simmer.plot)
library(data.table)
library(ggplot2)
library(janitor)

set.seed(42)
env <- simmer()

# Patient joins trial on on O2
# Patient passes through multiple shifts (consider these as 8 clinicians A-H)
#   - ordering of clinicians is 'random' 
#   - clinicians have different probabilities of intubating which is then adjusted by the nudge
# At each shift the patient's status O2/CPAP/Vent (O,C,V) might change
# At each shift the patient might die


intubate_yn <- function() {
  yn <- sample(1:2, 1 )
  # return as an integer to branch
  # 1=no # 2=yes 
  return(yn)
}

patient <- 
  trajectory("trial path")  %>% 
  set_attribute("resp_failure_score", function() {rnorm(1, mean=25, sd=5)} ) %>% 
  set_attribute("ventilated", 0 ) %>% 
  log_(function() paste(
                        "Resp failure score:",
                        get_attribute(TRIAL,"resp_failure_score"),
                        "Ventilated:",
                        get_attribute(TRIAL,"ventilated")
                        )) %>% 
  # define your recruitment rate (get clever later based on pandemic stage)
  log_("Arrive into a CPAP bed") %>% 
  seize("bed", 1) %>%  # use one bed
  set_attribute("resp_failure_score", function() {rnorm(1, mean=25, sd=5)} ) %>% 
  log_(function() paste(
                        "Resp failure score:",
                        get_attribute(TRIAL,"resp_failure_score"),
                        "Ventilated:",
                        get_attribute(TRIAL,"ventilated")
                        )) %>% 
  # 12 hour shift
  timeout(12) %>% 
  log_("Leaving CPAP bed") %>% 
  release("bed", 1)  %>%   
  
  branch(intubate_yn,
         continue = c(TRUE,TRUE),
         # branch 1 (not intubated): 
         trajectory() %>% 
           log_("Not intubated") %>% 
           set_attribute("ventilated", 1 ),
         # branch 2 (intubated): 
         trajectory() %>% 
           log_("Intubated")
         ) %>% 

  rollback(7, times=8) # loop back n steps and repeat


  
TRIAL <- simmer()
TRIAL %>% 
  add_resource("bed", 1) %>% 
  add_generator("patient", patient, at(0)) # add a new patient every 12h
  
TRIAL %>% 
  reset() %>% 
  run(until=96) %>% 
  get_mon_arrivals()

dt <- setDT(TRIAL %>% get_mon_arrivals(per_resource=TRUE)
            %>% transform(foo=get_attribute(ventilated)))
  

TRIAL %>% get_mon_attributes() 
