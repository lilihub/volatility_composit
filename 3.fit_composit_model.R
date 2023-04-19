rm(list=ls())
setwd("~/volatility_composit")
library(rstan)
library(bayesplot)
library(shinystan)
library(ggplot2)
library(cowplot)
library(abind)
library(dplyr)
library(ggpubr)
library(tidyr)
source("utils/fit_composit_functions.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

###### if you wanna fit the model on your own machine, run this block ########
CBT_baseline_composit1= fit_composit_model(choice_file = "CBT_baseline_matched", 
                                           clinical_file = "clinical_CBT_baseline", 
                                           nC = 0,
                                           cov_name = "",
                                           model_name = "alt_composit1.stan",
                                           save_name = "CBT_baseline_composit1")

CBT_baseline_composit1.1= fit_composit_model(choice_file = "CBT_baseline_matched", 
                                             clinical_file = "clinical_CBT_baseline", 
                                             nC = 0,
                                             cov_name = "",
                                             model_name = "alt_composit1.1.stan",
                                             save_name = "CBT_baseline_composit1.1")

CBT_baseline_composit2= fit_composit_model(choice_file = "CBT_baseline_matched", 
                                           clinical_file = "clinical_CBT_baseline", 
                                           nC = 1,
                                           cov_name = "AD",
                                           model_name = "alt_composit2.stan",
                                           save_name = "CBT_baseline_composit2")
CBT_baseline_composit2.1 = fit_composit_model(choice_file = "CBT_baseline_matched", 
                                              clinical_file = "clinical_CBT_baseline_scale", 
                                              nC = 1,
                                              cov_name = "STAI_total",
                                              model_name = "alt_composit2.1.stan",
                                              save_name = "CBT_baseline_composit2.1")

CBT_followup_composit2.1 = fit_composit_model(choice_file = "CBT_followup_matched", 
                                              clinical_file = "clinical_CBT_followup_scale", 
                                              nC = 1,
                                              cov_name = "STAI_total",
                                              model_name = "alt_composit2.1.stan",
                                              save_name = "CBT_followup_composit2.1")










CBT_baseline_stable = readRDS("fitted_data/CBT_baseline_stable")
CBT_baseline_volatile = readRDS("fitted_data/CBT_baseline_volatile")
CBT_followup_stable = readRDS("fitted_data/CBT_followup_stable")
CBT_followup_volatile = readRDS("fitted_data/CBT_followup_volatile")


mike_schedule1_stable <- readRDS("fitted_data/mike_schedule1_stable.rda")
mike_schedule1_volatile <- readRDS("fitted_data/mike_schedule1_volatile.rda")
mike_schedule2_stable <- readRDS("fitted_data/mike_schedule2_stable.rda")
mike_schedule2_volatile <- readRDS("fitted_data/mike_schedule2_volatile.rda")
mike_stable <- readRDS("fitted_data/mike_stable.rda")
mike_volatile <- readRDS("fitted_data/mike_volatile.rda")

mike_schedule1_stable_append <- readRDS("fitted_data/mike_schedule1_stable_append.rda")
mike_schedule1_volatile_append <- readRDS("fitted_data/mike_schedule1_volatile_append.rda")
mike_schedule2_stable_append <- readRDS("fitted_data/mike_schedule2_stable_append.rda")
mike_schedule2_volatile_append <- readRDS("fitted_data/mike_schedule2_volatile_append.rda")

## posterior predictive checks refer to when a fitted model is used to generate simulated
## data and check if simulated data are similar to the actual data. Posterior predictive 
## checks are useful in assessing if a model generates valid predictions.
## ** Important link that explains ppc **
## https://stats.stackexchange.com/questions/115157/what-are-posterior-predictive-checks-and-what-makes-them-useful
## https://mc-stan.org/bayesplot/reference/PPC-overview.html
## The posterior predictive distribution is the ** distribution over new observations
## given previous observations **.

## posterior predictive check for the choices of each subject in each trial
# CBT baseline schedule1
ppc_choice(CBT_baseline_schedule1_stable[[3]], CBT_baseline_schedule1_stable[[1]], 
           CBT_baseline_schedule1_stable[[2]], "a_Cbase_schedule1_stable/")
ppc_choice(CBT_baseline_schedule1_volatile[[3]], CBT_baseline_schedule1_volatile[[1]], 
           CBT_baseline_schedule1_volatile[[2]], "a_Cbase_schedule1_volatile/")
# CBT baseline schedule2
ppc_choice(CBT_baseline_schedule2_stable[[3]], CBT_baseline_schedule2_stable[[1]], 
           CBT_baseline_schedule2_stable[[2]], "a_Cbase_schedule2_stable/")
ppc_choice(CBT_baseline_schedule2_volatile[[3]], CBT_baseline_schedule2_volatile[[1]], 
           CBT_baseline_schedule2_volatile[[2]], "a_Cbase_schedule2_volatile/")
# CBT followup schedule1
ppc_choice(CBT_followup_schedule1_stable[[3]], CBT_followup_schedule1_stable[[1]], 
           CBT_followup_schedule1_stable[[2]], "a_Cfoll_schedule1_stable/")
ppc_choice(CBT_followup_schedule1_volatile[[3]], CBT_followup_schedule1_volatile[[1]], 
           CBT_followup_schedule1_volatile[[2]], "a_Cfoll_schedule1_volatile/")
# CBT followup schedule2
ppc_choice(CBT_followup_schedule2_stable[[3]], CBT_followup_schedule2_stable[[1]], 
           CBT_followup_schedule2_stable[[2]], "a_Cfoll_schedule2_stable/")
ppc_choice(CBT_followup_schedule2_volatile[[3]], CBT_followup_schedule2_volatile[[1]], 
           CBT_followup_schedule2_volatile[[2]], "a_Cfoll_schedule2_volatile/")

# SSRI baseline schedule1
ppc_choice(SSRI_baseline_schedule1_stable[[3]], SSRI_baseline_schedule1_stable[[1]], 
           SSRI_baseline_schedule1_stable[[2]], "a_Sbase_schedule1_stable/")
ppc_choice(SSRI_baseline_schedule1_volatile[[3]], SSRI_baseline_schedule1_volatile[[1]], 
           SSRI_baseline_schedule1_volatile[[2]], "a_Sbase_schedule1_volatile/")
# SSRI baseline schedule2
ppc_choice(SSRI_baseline_schedule2_stable[[3]], SSRI_baseline_schedule2_stable[[1]], 
           SSRI_baseline_schedule2_stable[[2]], "a_Sbase_schedule2_stable/")
ppc_choice(SSRI_baseline_schedule2_volatile[[3]], SSRI_baseline_schedule2_volatile[[1]], 
           SSRI_baseline_schedule2_volatile[[2]], "a_Sbase_schedule2_volatile/")
# SSRI followup schedule1
ppc_choice(SSRI_followup_schedule1_stable[[3]], SSRI_followup_schedule1_stable[[1]], 
           SSRI_followup_schedule1_stable[[2]], "a_Sfoll_schedule1_stable/")
ppc_choice(SSRI_followup_schedule1_volatile[[3]], SSRI_followup_schedule1_volatile[[1]], 
           SSRI_followup_schedule1_volatile[[2]], "a_Sfoll_schedule1_volatile/")
# SSRI followup schedule2
ppc_choice(SSRI_followup_schedule2_stable[[3]], SSRI_followup_schedule2_stable[[1]], 
           SSRI_followup_schedule2_stable[[2]], "a_Sfoll_schedule2_stable/")
ppc_choice(SSRI_followup_schedule1_volatile[[3]], SSRI_followup_schedule2_volatile[[1]], 
           SSRI_followup_schedule2_volatile[[2]], "a_Sfoll_schedule2_volatile/")

## posterior predictive check for the correct choices of all subjects in each trial and for each subjects in all trials
## the plots are saved in plots/ folder
CBT_baseline_schedule1_stable_ppc <- ppc_stable(CBT_baseline_schedule1_stable[[3]], 
                                                CBT_baseline_schedule1_stable[[2]], 1, 
                                                "Cbase_schedule1_stable_ppc")
CBT_baseline_schedule1_volatile_ppc <- ppc_volatile(CBT_baseline_schedule1_volatile[[3]],
                                                  CBT_baseline_schedule1_volatile[[2]], 1,
                                                  "Cbase_schedule1_volatile_ppc")
CBT_baseline_schedule2_stable_ppc <- ppc_stable(CBT_baseline_schedule2_stable[[3]], 
                                                CBT_baseline_schedule2_stable[[2]], 2,
                                                "Cbase_schedule2_stable_ppc")
CBT_baseline_schedule2_volatile_ppc <- ppc_volatile(CBT_baseline_schedule2_volatile[[3]],
                                                  CBT_baseline_schedule2_volatile[[2]], 2, 
                                                  "Cbase_schedule2_volatile_ppc")
CBT_followup_schedule1_stable_ppc <- ppc_stable(CBT_followup_schedule1_stable[[3]], 
                                                CBT_followup_schedule1_stable[[2]], 1, 
                                                "Cfoll_schedule1_stable_ppc")
CBT_followup_schedule1_volatile_ppc <- ppc_volatile(CBT_followup_schedule1_volatile[[3]], 
                                                  CBT_followup_schedule1_volatile[[2]], 1,
                                                  "Cfoll_schedule1_volatile_ppc")
CBT_followup_schedule2_stable_ppc <- ppc_stable(CBT_followup_schedule2_stable[[3]], 
                                                CBT_followup_schedule2_stable[[2]], 2, 
                                                "Cfoll_schedule2_stable_ppc")
CBT_followup_schedule2_volatile_ppc <- ppc_volatile(CBT_followup_schedule2_volatile[[3]], 
                                                  CBT_followup_schedule2_volatile[[2]], 2, 
                                                  "Cfoll_schedule2_volatile_ppc")

SSRI_baseline_schedule1_stable_ppc <- ppc_stable(SSRI_baseline_schedule1_stable[[3]], 
                                                SSRI_baseline_schedule1_stable[[2]], 1,
                                                "Sbase_schedule1_stable_ppc")
SSRI_baseline_schedule1_volatile_ppc <- ppc_volatile(SSRI_baseline_schedule1_volatile[[3]],
                                                     SSRI_baseline_schedule1_volatile[[2]], 1,
                                                     "Sbase_schedule1_volatile_ppc")
SSRI_baseline_schedule2_stable_ppc <- ppc_stable(SSRI_baseline_schedule2_stable[[3]],
                                                 SSRI_baseline_schedule2_stable[[2]], 2,
                                                 "Sbase_schedule2_stable_ppc")
SSRI_baseline_schedule2_volatile_ppc <- ppc_volatile(SSRI_baseline_schedule2_volatile[[3]],
                                                     SSRI_baseline_schedule2_volatile[[2]], 2,
                                                     "Sbase_schedule2_volatile_ppc")
SSRI_followup_schedule1_stable_ppc <-  ppc_stable(SSRI_followup_schedule1_stable[[3]], 
                                                  SSRI_followup_schedule1_stable[[2]], 1,
                                                  "Sfoll_schedule1_stable_ppc")
SSRI_followup_schedule1_volatile_ppc <- ppc_volatile(SSRI_followup_schedule1_volatile[[3]],
                                                     SSRI_followup_schedule1_volatile[[2]], 1,
                                                     "Sfoll_schedule1_volatile_ppc")
SSRI_followup_schedule2_stable_ppc <- ppc_stable(SSRI_followup_schedule2_stable[[3]], 
                                                 SSRI_followup_schedule2_stable[[2]], 2,
                                                 "Sfoll_schedule2_stable_ppc")
SSRI_followup_schedule2_volatile_ppc <- ppc_volatile(SSRI_followup_schedule2_volatile[[3]],
                                                     SSRI_followup_schedule2_volatile[[2]], 2,
                                                     "Sfoll_schedule2_volatile_ppc")

## the correct choice for subjects doing the same schedule is even different, so
## it is hard to do ppc for fitted model, which obtained from dataset that removed
## missing trials directly. Even I did ppc here, the results are actually not accurate
mike_schedule1_stable_ppc <- mike_ppc_stable(mike_schedule1_stable[[3]], 
                                        mike_schedule1_stable[[2]],
                                        "mike_schedule1_stable_ppc")
mike_schedule2_stable_ppc <- mike_ppc_stable(mike_schedule2_stable[[3]], 
                                             mike_schedule2_stable[[2]],
                                             "mike_schedule2_stable_ppc")
mike_schedule1_volatile_ppc <- mike_ppc_volatile(mike_schedule1_volatile[[3]],
                                                 mike_schedule1_volatile[[2]], 1,
                                                 "mike_schedule1_volatile_ppc")
mike_schedule2_volatile_ppc <- mike_ppc_volatile(mike_schedule2_volatile[[3]],
                                                 mike_schedule2_volatile[[2]], 2,
                                                 "mike_schedule2_volatile_ppc")
mike_stable_ppc <- mike_ppc_stable(mike_stable[[3]],
                                   mike_stable[[2]],
                                   "mike_stable_ppc")
mike_volatile_ppc <- mike_ppc_volatile(mike_volatile[[3]], ## this one has errors because the correct 
                                   mike_volatile[[2]],     ## choice are different in volatile phase 
                                   "mike_volatile_ppc")    ## between schedule1 and schedule2. need a more
                                                           ## complex function to differentiate the two schedules
                                                           ## when applying to combined dataset
mike_schedule1_stable_append_ppc <- mike_ppc_stable(mike_schedule1_stable_append[[3]], 
                                        mike_schedule1_stable_append[[2]], 
                                        "mike_schedule1_stable_append_ppc")
mike_schedule1_volatile_append_ppc <- mike_ppc_volatile(mike_schedule1_volatile_append[[3]],
                                            mike_schedule1_volatile_append[[2]], 1,
                                            "mike_schedule1_volatile_append_ppc")

mike_schedule2_stable_append_ppc <- mike_ppc_stable(mike_schedule2_stable_append[[3]], 
                                                    mike_schedule2_stable_append[[2]], 
                                                    "mike_schedule2_stable_append_ppc")
mike_schedule2_volatile_append_ppc <- mike_ppc_volatile(mike_schedule2_volatile_append[[3]],
                                                        mike_schedule2_volatile_append[[2]], 2,
                                                        "mike_schedule2_volatile_append_ppc")

