library(rstan)
library(cowplot)
library(stringr)
library(rstatix)
library(tidyverse)
############################ Control group #################################
##### read in model fits #####
control_baseline_composit0 <- readRDS("fitted_data/chris_model2_composit0/control_baseline_fit.rda")
control_baseline_composit0_pars <- rstan::extract(control_baseline_composit0)

control_baseline_composit1 <- readRDS("fitted_data/chris_model2_composit1/control_baseline_fit.rda")
control_baseline_composit1_pars <- rstan::extract(control_baseline_composit1)

control_baseline_composit2 <- readRDS("fitted_data/chris_model2_composit2/control_baseline_fit.rda")
control_baseline_composit2_pars <- rstan::extract(control_baseline_composit2)

control_baseline_composit3 <- readRDS("fitted_data/chris_model2_composit3/control_baseline_fit.rda")
control_baseline_composit3_pars <- rstan::extract(control_baseline_composit3)

control_baseline_composit4 <- readRDS("fitted_data/chris_model2_composit4/control_baseline_fit.rda")
control_baseline_composit4_pars <- rstan::extract(control_baseline_composit4)

######### Group-level parameters #########
control_baseline_composit0_par = data.frame(composit0_A = control_baseline_composit0_pars[['mu_pr']][, 1],
                                            composit0_beta= control_baseline_composit0_pars[['mu_beta']],
                                            composit0_gamma = control_baseline_composit0_pars[['mu_gamma']])

control_baseline_composit1_par = data.frame(composit1_A_base = control_baseline_composit1_pars[['mu_pr']][, 1],
                                            composit1_A_vs = control_baseline_composit1_pars[['mu_pr']][, 2],
                                            composit1_beta= control_baseline_composit1_pars[['mu_beta']],
                                            composit1_gamma = control_baseline_composit1_pars[['mu_gamma']])

control_baseline_composit2_par = data.frame(composit2_A_base = control_baseline_composit2_pars[['mu_pr']][, 1],
                                            composit2_A_gb = control_baseline_composit2_pars[['mu_pr']][, 2],
                                            composit2_beta= control_baseline_composit2_pars[['mu_beta']],
                                            composit2_gamma = control_baseline_composit2_pars[['mu_gamma']])

control_baseline_composit3_par = data.frame(composit3_A_base = control_baseline_composit3_pars[['mu_pr']][, 1],
                                            composit3_A_vs = control_baseline_composit3_pars[['mu_pr']][, 2],
                                            composit3_A_gb = control_baseline_composit3_pars[['mu_pr']][, 3],
                                            composit3_beta= control_baseline_composit3_pars[['mu_beta']],
                                            composit3_gamma = control_baseline_composit3_pars[['mu_gamma']])

control_baseline_composit4_par = data.frame(composit4_A_base = control_baseline_composit4_pars[['mu_pr']][, 1],
                                            composit4_A_vs = control_baseline_composit4_pars[['mu_pr']][, 2],
                                            composit4_A_gb = control_baseline_composit4_pars[['mu_pr']][, 3],
                                            composit4_A_vs_gb = control_baseline_composit4_pars[['mu_pr']][, 4],
                                            composit4_beta= control_baseline_composit4_pars[['mu_beta']],
                                            composit4_gamma = control_baseline_composit4_pars[['mu_gamma']])

control_baseline_par = cbind(control_baseline_composit0_par, control_baseline_composit1_par,
                             control_baseline_composit2_par, control_baseline_composit3_par,
                             control_baseline_composit4_par)
write.csv(control_baseline_par, 'fitted_data/chris_model2_composit/control_baseline_par.csv', row.names = FALSE)



############### Individual-level parameters ###############
##### CBT #####
control_baseline = read.csv('clean_data/control_baseline_matched.csv')
subjID_baseline = unique(CBT_baseline$subjID)
control_baseline_composit0_indpar = data.frame(subjID = subjID_baseline,
                                               composit0_A = colMeans(control_baseline_composit0_pars[['A_base']]),
                                               composit0_beta = colMeans(control_baseline_composit0_pars[['beta']]),
                                               composit0_gamma = colMeans(control_baseline_composit0_pars[['gamma']]))

control_baseline_composit1_indpar = data.frame(subjID = subjID_baseline,
                                               composit1_A_base = colMeans(control_baseline_composit1_pars[['A_base']]),
                                               composit1_A_vs = colMeans(control_baseline_composit1_pars[['A_vs']]),
                                               composit1_beta = colMeans(control_baseline_composit1_pars[['beta']]),
                                               composit1_gamma = colMeans(control_baseline_composit1_pars[['gamma']]))

control_baseline_composit2_indpar = data.frame(subjID = subjID_baseline,
                                               composit2_A_base = colMeans(control_baseline_composit2_pars[['A_base']]),
                                               composit2_A_gb = colMeans(control_baseline_composit2_pars[['A_gb']]),
                                               composit2_beta = colMeans(control_baseline_composit2_pars[['beta']]),
                                               composit2_gamma = colMeans(control_baseline_composit2_pars[['gamma']]))


control_baseline_composit3_indpar = data.frame(subjID = subjID_baseline,
                                               composit3_A_base = colMeans(control_baseline_composit3_pars[['A_base']]),
                                               composit3_A_vs = colMeans(control_baseline_composit3_pars[['A_vs']]),
                                               composit3_A_gb = colMeans(control_baseline_composit3_pars[['A_gb']]),
                                               composit3_beta = colMeans(control_baseline_composit3_pars[['beta']]),
                                               composit3_gamma = colMeans(control_baseline_composit3_pars[['gamma']]))

control_baseline_composit4_indpar = data.frame(subjID = subjID_baseline,
                                               composit4_A_base = colMeans(control_baseline_composit4_pars[['A_base']]),
                                               composit4_A_vs = colMeans(control_baseline_composit4_pars[['A_vs']]),
                                               composit4_A_gb = colMeans(control_baseline_composit4_pars[['A_gb']]),
                                               composit4_A_vs_gb = colMeans(control_baseline_composit4_pars[['A_vs_gb']]),
                                               composit4_beta = colMeans(control_baseline_composit4_pars[['beta']]),
                                               composit4_gamma = colMeans(control_baseline_composit4_pars[['gamma']]))


control_baseline_indpar_list <- list(control_baseline_composit0_indpar, control_baseline_composit1_indpar, 
                                     control_baseline_composit2_indpar, control_baseline_composit3_indpar,
                                     control_baseline_composit4_indpar)
control_baseline_indpar = control_baseline_indpar_list %>% reduce(full_join, by='subjID')
write.csv(control_baseline_indpar, 'fitted_data/chris_model2_composit/control_baseline_indpar.csv', row.names = FALSE)


##### control #####
control_baseline_stable_mu = as.data.frame(control_baseline_stable_pars[9:11])
colnames(control_baseline_stable_mu) = c('baseline_A_stable', 'baseline_beta_stable', 'baseline_gamma_stable')
control_baseline_volatile_mu = as.data.frame(control_baseline_volatile_pars[9:11])
colnames(control_baseline_volatile_mu) = c('baseline_A_volatile', 'baseline_beta_volatile', 'baseline_gamma_volatile')

control_followup_stable_mu = as.data.frame(control_followup_stable_pars[9:11])
colnames(control_followup_stable_mu) = c('followup_A_stable', 'followup_beta_stable', 'followup_gamma_stable')
control_followup_volatile_mu = as.data.frame(control_followup_volatile_pars[9:11])
colnames(control_followup_volatile_mu) = c('followup_A_volatile', 'followup_beta_volatile', 'followup_gamma_volatile')
control_followup_mu = cbind(control_followup_stable_mu, control_followup_volatile_mu)

control_mu = cbind(control_baseline_stable_mu, control_baseline_volatile_mu, control_followup_stable_mu, control_followup_volatile_mu)
write.csv(control_mu, "fitted_data/control_mu.csv", row.names = FALSE)













######################### CBT convergence check ####################################
CBT_baseline_composit <- readRDS("fitted_data/CBT_baseline_composit2_fit.rda")
CBT_baseline_summary <- rstan::summary(CBT_baseline_composit)
CBT_baseline_rhat <- data.frame(Rhat = CBT_baseline_summary[["summary"]][, "Rhat"])
CBT_baseline_unnormal_rhat <- subset(CBT_baseline_rhat, CBT_baseline_rhat$Rhat>1.1)
hist(CBT_baseline_rhat$Rhat)

CBT_baseline_composit_pars <- rstan::extract(CBT_baseline_composit)
mu = as.data.frame(CBT_baseline_composit_pars[1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_trace(mu,  pars=c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           facet_args = list(nrow = 2, labeller = label_parsed))



CBT_followup_composit <- readRDS("fitted_data/CBT_followup_composit2_fit.rda")
CBT_followup_summary <- rstan::summary(CBT_followup_composit)
CBT_followup_rhat <- data.frame(Rhat = CBT_followup_summary[["summary"]][, "Rhat"])
CBT_followup_unnormal_rhat <- subset(CBT_followup_rhat, CBT_followup_rhat$Rhat>1.1)
hist(CBT_followup_rhat$Rhat)

CBT_followup_composit_pars <- rstan::extract(CBT_followup_composit)
mu = as.data.frame(CBT_followup_composit_pars[1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_trace(mu,  pars=c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           facet_args = list(nrow = 2, labeller = label_parsed))


