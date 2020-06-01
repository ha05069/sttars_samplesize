library(tidyverse)
library(naniar)


# Re-draft from simulation script 5 to incorporate:

  # 1. Feedback from Ed
        # Incorporate evaluative function at end of sim - a statistical test which evaluates tx success or failure
        # Abstract randomisation/equipoise sim steps into a separate function
        # Add steps to simulate 1000s of times
  # 2. Slightly altered protocol 1
  # 3. New design proposed by Roma
  # 4. Write up in format suggested by @morris2019


# Refs:

# https://github.com/DocEd/fragility-index/blob/master/code/fragility-sim.R
# http://arxiv.org/abs/1712.03198 (@morris2019)


# Protocol 1 

  # This is the original protocol put forward which suggests randomisation point every 12 hours
  # Re-doing the code with help from Ed's fragility sim study and additional notes from @morris2019

## Simulation studies are experiments which aim to obtain empirical results about 


set.seed(2019)

#' Calculate Average Treatment Effect
#' 
#' returns the average treatment effect given treatment assignment in a
#' randomised scenario
#'
#' @param t treatment assignment (must be 0 or 1)
#' @param y outcome (must be 0 or 1)

ate_calc <- function(t, y) {
  mean(y[t == 1]) - mean(y[t == 0])
}


# think about the parameters that will be required within the function:
#' @param n_obs a manually selected total number of participants in the simulation
#' @param n_sims the number of simulations to run (default 1000) 

#'
#' @param intervention_mort treatment arm mortality (probability scalar)
#' @param power desired statistical power (probability scalar)
#' @param control_mort control arm mortality (probability scalar)
#' @param n manual selection of subjects in total (overrides other settings)
#' @param sims number of sims to run (default 1000)
#' @param alpha pre-fixed alpha level to run fragility index
#' @param permit_negative allow negative fragility index

