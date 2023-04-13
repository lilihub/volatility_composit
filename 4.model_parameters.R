library(rstan)
library(cowplot)
library(stringr)
library(rstatix)
#############################################################
CBT_baseline_composit <- readRDS("fitted_data/CBT_baseline_composit1_fit.rda")
CBT_baseline_composit_pars <- rstan::extract(CBT_baseline_composit)
CBT_baseline = read.csv('clean_data/CBT_baseline_matched.csv')
subjID = unique(CBT_baseline$subjID)
# subjID = subjID[!(subjID %in% 'participant41')]
CBT_baseline_composit_par = data.frame(subjID = subjID,
                        A_base = colMeans(CBT_baseline_composit_pars[['A_base']]),
                        A_vs = colMeans(CBT_baseline_composit_pars[['A_vs']]),
                        A_gb = colMeans(CBT_baseline_composit_pars[['A_gb']]),
                        beta = colMeans(CBT_baseline_composit_pars[['beta']]),
                        gamma = colMeans(CBT_baseline_composit_pars[['gamma']]))
write.csv(pdpain_composit_par, 'clean_data/pdpain_composit_par.csv', row.names = FALSE)

s