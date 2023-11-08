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
source("utils/fit_separate_functions.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

### mike_baseline_updated_ev (record the ev values)
### control
mike_baseline_updated_ev_control_baseline_schedule1_stable = fit_model("control_baseline_schedule1_stable_matched", 
                                                                       "mike_baseline_updated_ev.stan", 
                                                                       "mike_baseline_updated_ev/control_baseline_schedule1_stable")
ev = rstan::extract(mike_baseline_updated_ev_control_baseline_schedule1_stable)$ev
ev_mean = colMeans(ev)
write.csv(ev_mean, "fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_stable_ev.csv", row.names = FALSE)
mike_baseline_updated_ev_control_baseline_schedule1_volatile = fit_model("control_baseline_schedule1_volatile_matched", 
                                                                         "mike_baseline_updated_ev.stan", 
                                                                         "mike_baseline_updated_ev/control_baseline_schedule1_volatile")


mike_baseline_updated_ev_control_baseline_schedule2_volatile = fit_model("control_baseline_schedule2_volatile_matched", 
                                                                         "mike_baseline_updated_ev.stan", 
                                                                         "mike_baseline_updated_ev/control_baseline_schedule2_volatile")
ev = rstan::extract(mike_baseline_updated_ev_control_baseline_schedule2_volatile)$ev
ev_mean = colMeans(ev)
write.csv(ev_mean, "fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_volatile_ev.csv", row.names = FALSE)
mike_baseline_updated_ev_control_baseline_schedule2_stable = fit_model("control_baseline_schedule2_stable_matched", 
                                                                       "mike_baseline_updated_ev.stan", 
                                                                       "mike_baseline_updated_ev/control_baseline_schedule2_stable")

######### chris model2_ev #########
### need to modify the fit_separate_model_function to feed it with updated ev
### control
chris_model2_ev_control_baseline_schedule1_stable = fit_model("control_baseline_schedule1_stable_matched", 
                                                           "chris_model2_ev.stan", 
                                                           "chris_model2_ev/control_baseline_schedule1_stable")
ev = rstan::extract(chris_model2_ev_control_baseline_schedule1_stable)$ev
ev_mean = colMeans(ev)
write.csv(ev_mean, "fitted_data/chris_model2_ev/control_baseline_schedule1_stable_ev.csv", row.names = FALSE)

chris_model2_ev_control_baseline_schedule1_volatile = fit_model("control_baseline_schedule1_volatile_matched", 
                                                             "chris_model2_ev.stan", 
                                                             "chris_model2_ev/control_baseline_schedule1_volatile")

chris_model2_ev_control_baseline_schedule2_volatile = fit_model("control_baseline_schedule2_volatile_matched", 
                                                             "chris_model2_ev.stan", 
                                                             "chris_model2_ev/control_baseline_schedule2_volatile")
ev = rstan::extract(chris_model2_ev_control_baseline_schedule2_volatile)$ev
ev_mean = colMeans(ev)
write.csv(ev_mean, "fitted_data/chris_model2_ev/control_baseline_schedule2_volatile_ev.csv", row.names = FALSE)

chris_model2_ev_control_baseline_schedule2_stable = fit_model("control_baseline_schedule2_stable_matched", 
                                                              "chris_model2_ev.stan", 
                                                              "chris_model2_ev/control_baseline_schedule2_stable")

######### chris model2
### control
chris_model2_control_baseline_schedule1_stable = fit_model("control_baseline_schedule1_stable_matched", 
                                                           "chris_model2.stan", 
                                                           "chris_model2/control_baseline_schedule1_stable")
chris_model2_control_baseline_schedule1_volatile = fit_model("control_baseline_schedule1_volatile_matched", 
                                                             "chris_model2.stan", 
                                                             "chris_model2/control_baseline_schedule1_volatile")

chris_model2_control_baseline_schedule2_stable = fit_model("control_baseline_schedule2_stable_matched", 
                                                           "chris_model2.stan", 
                                                           "chris_model2/control_baseline_schedule2_stable")
chris_model2_control_baseline_schedule2_volatile = fit_model("control_baseline_schedule2_volatile_matched", 
                                                             "chris_model2.stan", 
                                                             "chris_model2/control_baseline_schedule2_volatile")


### mike_baseline (the original npc model)
### control
mike_baseline_control_baseline_schedule1_stable = fit_model("control_baseline_schedule1_stable_matched", 
                                                            "mike_baseline.stan", 
                                                            "mike_baseline/control_baseline_schedule1_stable")
mike_baseline_control_baseline_schedule1_volatile = fit_model("control_baseline_schedule1_volatile_matched", 
                                                              "mike_baseline.stan", 
                                                              "mike_baseline/control_baseline_schedule1_volatile")
mike_baseline_control_baseline_schedule2_stable = fit_model("control_baseline_schedule2_stable_matched", 
                                                            "mike_baseline.stan", 
                                                            "mike_baseline/control_baseline_schedule2_stable")
mike_baseline_control_baseline_schedule2_volatile = fit_model("control_baseline_schedule2_volatile_matched", 
                                                              "mike_baseline.stan", 
                                                              "mike_baseline/control_baseline_schedule2_volatile")

### mike_baseline_updated (the modified npc model)
### control
mike_baseline_updated_control_baseline_schedule1_stable = fit_model("control_baseline_schedule1_stable_matched", 
                                                                    "mike_baseline_updated.stan", 
                                                                    "mike_baseline_updated/control_baseline_schedule1_stable")
mike_baseline_updated_control_baseline_schedule1_volatile = fit_model("control_baseline_schedule1_volatile_matched", 
                                                                      "mike_baseline_updated.stan", 
                                                                      "mike_baseline_updated/control_baseline_schedule1_volatile")
mike_baseline_updated_control_baseline_schedule2_stable = fit_model("control_baseline_schedule2_stable_matched", 
                                                                    "mike_baseline_updated.stan", 
                                                                    "mike_baseline_updated/control_baseline_schedule2_stable")
mike_baseline_updated_control_baseline_schedule2_volatile = fit_model("control_baseline_schedule2_volatile_matched", 
                                                                      "mike_baseline_updated.stan", 
                                                                      "mike_baseline_updated/control_baseline_schedule2_volatile")



# CBT_baseline_stable = fit_model("CBT_baseline_stable")
# CBT_baseline_volatile = fit_model("CBT_baseline_volatile")
# CBT_followup_stable = fit_model("CBT_followup_stable")
# CBT_followup_volatile = fit_model("CBT_followup_volatile")
# 
# 
# SSRI_baseline_stable = fit_model("SSRI_baseline_stable")
# SSRI_baseline_volatile = fit_model("SSRI_baseline_volatile")
# SSRI_followup_stable = fit_model("SSRI_followup_stable")
# SSRI_followup_volatile = fit_model("SSRI_followup_volatile")
# 
# control_baseline_stable = fit_model("control_baseline_stable")
# control_baseline_volatile = fit_model("control_baseline_volatile")
# control_followup_stable = fit_model("control_followup_stable")
# control_followup_volatile = fit_model("control_followup_volatile")


