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
CBT_baseline_composit0= fit_composit_model(choice_file = "CBT_baseline_matched",
                                           clinical_file = "clinical_CBT_baseline",
                                           nC = 0,
                                           cov_name = "",
                                           model_name = "chris_model2_composit0.stan",
                                           save_name = "chris_model2_composit0/CBT_baseline")
# CBT_followup_composit0= fit_composit_model(choice_file = "CBT_followup_matched",
#                                            clinical_file = "clinical_CBT_baseline",
#                                            nC = 0,
#                                            cov_name = "",
#                                            model_name = "alt_composit0.stan",
#                                            save_name = "CBT_followup_composit0")
control_baseline_composit0= fit_composit_model(choice_file = "control_baseline_matched",
                                            clinical_file = "clinical_control_baseline",
                                            nC = 0,
                                            cov_name = "",
                                            model_name = "chris_model2_composit0.stan",
                                            save_name = "chris_model2_composit0/control_baseline")
control_baseline_composit1= fit_composit_model(choice_file = "control_baseline_matched",
                                               clinical_file = "clinical_control_baseline",
                                               nC = 0,
                                               cov_name = "",
                                               model_name = "chris_model2_composit1.stan",
                                               save_name = "chris_model2_composit1/control_baseline")
control_baseline_composit2= fit_composit_model(choice_file = "control_baseline_matched",
                                               clinical_file = "clinical_control_baseline",
                                               nC = 0,
                                               cov_name = "",
                                               model_name = "chris_model2_composit2.stan",
                                               save_name = "chris_model2_composit2/control_baseline")
control_baseline_composit3= fit_composit_model(choice_file = "control_baseline_matched",
                                               clinical_file = "clinical_control_baseline",
                                               nC = 0,
                                               cov_name = "",
                                               model_name = "chris_model2_composit3.stan",
                                               save_name = "chris_model2_composit3/control_baseline")
control_baseline_composit4= fit_composit_model(choice_file = "control_baseline_matched",
                                               clinical_file = "clinical_control_baseline",
                                               nC = 0,
                                               cov_name = "",
                                               model_name = "chris_model2_composit4.stan",
                                               save_name = "chris_model2_composit4/control_baseline")

# SSRI_baseline_composit1= fit_composit_model(choice_file = "SSRI_baseline_matched",
#                                            clinical_file = "clinical_SSRI_baseline",
#                                            nC = 0,
#                                            cov_name = "",
#                                            model_name = "alt_composit1.stan",
#                                            save_name = "SSRI_baseline_composit1")
# SSRI_followup_composit1= fit_composit_model(choice_file = "SSRI_followup_matched",
#                                            clinical_file = "clinical_SSRI_followup",
#                                            nC = 0,
#                                            cov_name = "",
#                                            model_name = "alt_composit1.stan",
#                                            save_name = "SSRI_followup_composit1")
# control_baseline_composit1= fit_composit_model(choice_file = "control_baseline_matched",
#                                             clinical_file = "clinical_control_baseline",
#                                             nC = 0,
#                                             cov_name = "",
#                                             model_name = "alt_composit1.stan",
#                                             save_name = "control_baseline_composit1")
# control_followup_composit1= fit_composit_model(choice_file = "control_followup_matched",
#                                             clinical_file = "clinical_control_followup",
#                                             nC = 0,
#                                             cov_name = "",
#                                             model_name = "alt_composit1.stan",
#                                             save_name = "control_followup_composit1")

# CBT_baseline_composit2_nointer= fit_composit_model(choice_file = "CBT_baseline_matched",
#                                            clinical_file = "clinical_CBT_baseline",
#                                            nC = 1,
#                                            cov_name = "AD",
#                                            model_name = "alt_composit2_nointer.stan",
#                                            save_name = "CBT_baseline_composit2_nointer")
# CBT_followup_composit2_nointer= fit_composit_model(choice_file = "CBT_followup_matched",
#                                            clinical_file = "clinical_CBT_followup",
#                                            nC = 1,
#                                            cov_name = "AD_fu",
#                                            model_name = "alt_composit2_nointer.stan",
#                                            save_name = "CBT_followup_composit2_nointer")

CBT_baseline_basic_stable_volatile_lr = fit_composit_model(choice_file = "CBT_baseline_matched",
                                                   clinical_file = "clinical_CBT_baseline",
                                                   nC = 0,
                                                   cov_name = "",
                                                   model_name = "basicModel_stable_vol_lr.stan",
                                                   save_name = "CBT_baseline_basic_stable_volatile_lr")
CBT_followup_basic_stable_volatile_lr = fit_composit_model(choice_file = "CBT_followup_matched",
                                                   clinical_file = "clinical_CBT_followup",
                                                   nC = 0,
                                                   cov_name = "",
                                                   model_name = "basicModel_stable_vol_lr.stan",
                                                   save_name = "CBT_followup_basic_stable_volatile_lr")

control_baseline_basic_stable_volatile_lr = fit_composit_model(choice_file = "control_baseline_matched",
                                                           clinical_file = "clinical_control_baseline",
                                                           nC = 0,
                                                           cov_name = "",
                                                           model_name = "basicModel_stable_vol_lr.stan",
                                                           save_name = "control_baseline_basic_stable_volatile_lr")




CBT_baseline_composit2= fit_composit_model(choice_file = "CBT_baseline_matched",
                                           clinical_file = "clinical_CBT_baseline",
                                           nC = 1,
                                           cov_name = "AD",
                                           model_name = "alt_composit2.stan",
                                           save_name = "CBT_baseline_composit2")
CBT_followup_composit2= fit_composit_model(choice_file = "CBT_followup_matched",
                                           clinical_file = "clinical_CBT_followup",
                                           nC = 1,
                                           cov_name = "AD_fu",
                                           model_name = "alt_composit2.stan",
                                           save_name = "CBT_followup_composit2")
SSRI_baseline_composit2= fit_composit_model(choice_file = "SSRI_baseline_matched",
                                           clinical_file = "clinical_SSRI_baseline",
                                           nC = 1,
                                           cov_name = "AD",
                                           model_name = "alt_composit2.stan",
                                           save_name = "SSRI_baseline_composit2")
SSRI_followup_composit2= fit_composit_model(choice_file = "SSRI_followup_matched",
                                           clinical_file = "clinical_SSRI_followup",
                                           nC = 1,
                                           cov_name = "AD_fu",
                                           model_name = "alt_composit2.stan",
                                           save_name = "SSRI_followup_composit2")

control_baseline_composit2= fit_composit_model(choice_file = "control_baseline_matched",
                                            clinical_file = "clinical_control_baseline",
                                            nC = 1,
                                            cov_name = "AD",
                                            model_name = "alt_composit2.stan",
                                            save_name = "control_baseline_composit2")
control_followup_composit2= fit_composit_model(choice_file = "control_followup_matched",
                                            clinical_file = "clinical_control_followup",
                                            nC = 1,
                                            cov_name = "AD_fu",
                                            model_name = "alt_composit2.stan",
                                            save_name = "control_followup_composit2")


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

