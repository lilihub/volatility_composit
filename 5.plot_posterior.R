# This script is used to plot posterior distributions of the group-level parameters
rm(list=ls())
library(rstan)
library(reshape2)
library(cowplot)
library(knitr)
library(dplyr)
library(bayesplot)
library(loo)
library(ggplot2)

source("utils/stan_rhat.R")
source("utils/plot_diff_HDI_info.R")
source("utils/plot_diff.R")
source("utils/HDIofMCMC.R")
source("utils/plot_utils.R")

alpha <- intToUtf8(0x03B1)
beta <- intToUtf8(0x03B2)
gamma <- intToUtf8(0x03B3)

control_baseline_mu = read.csv("fitted_data/chris_model2_composit/control_baseline_par.csv")
mcmc_areas(control_baseline_mu, pars = c('composit0_A', 'composit1_A_base', 'composit2_A_base', 'composit3_A_base',
                                         'composit4_A_base'), prob = 0.95, prob_outer = 0.99,point_est = "mean") + labs(title = "A_base")

mcmc_areas(control_baseline_mu, pars = c('composit1_A_vs', 'composit2_A_gb', 'composit3_A_vs', 'composit3_A_gb', 'composit4_A_vs', 
                                         'composit4_A_gb', 'composit4_A_vs_gb'), 
           prob = 0.95, prob_outer = 0.99,point_est = "mean") + labs(title = "A_vs & A_gb")


mcmc_areas(control_baseline_mu, pars = c('composit0_beta', 'composit1_beta', 'composit2_beta', 'composit3_beta', 'composit4_beta'), 
           prob = 0.95, prob_outer = 0.99,point_est = "mean") + labs(title = "Beta")

mcmc_areas(control_baseline_mu, pars = c('composit0_gamma', 'composit1_gamma', 'composit2_gamma', 'composit3_gamma', 'composit4_gamma'), 
           prob = 0.95, prob_outer = 0.99,point_est = "mean") + labs(title = "gamma")





control_baseline_basic_stable_volatile_lr <- readRDS("fitted_data/control_baseline_basic_stable_volatile_lr_fit.rda")
control_baseline_basic_stable_volatile_lr_pars <- rstan::extract(control_baseline_basic_stable_volatile_lr)
mu = as.data.frame(control_baseline_basic_stable_volatile_lr_pars[1])
colnames(mu) = c('A_stable', 'A_volatile', 'beta', 'gamma')
mcmc_areas(mu, pars = c('A_stable', 'A_volatile', 'beta', 'gamma'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")
library(boot)
mu['A_stable'] = inv.logit(mu[,1])
mu['A_volatile'] = inv.logit(mu[,2])
mu['beta'] = inv.logit(mu[,3])
mu['gamma'] = inv.logit(mu[,4])
mcmc_areas(mu, pars = c('A_stable', 'A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")

library(boot)










mcmc_areas(chris_model2_mu, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_control_baseline_A")
mcmc_areas(chris_model2_mu, pars = c('schedule1_beta_stable', 'schedule1_beta_volatile', 'schedule2_beta_stable', 'schedule2_beta_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_control_baseline_beta")
mcmc_areas(chris_model2_mu, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_control_baseline_gamma")


mcmc_areas(mike_baseline_mu, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_control_baseline_A")
mcmc_areas(mike_baseline_mu, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_control_baseline_gamma")






mcmc_areas(mike_baseline_updated_mu, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_control_baseline_A")

mcmc_areas(mike_baseline_updated_mu, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_control_baseline_gamma")

mcmc_areas(mike_baseline_updated_mu, pars = c('schedule1_beta_stable', 'schedule1_beta_volatile', 'schedule2_beta_stable', 'schedule2_beta_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_control_baseline_beta")








CBT_baseline_composit1.1 <- readRDS("fitted_data/CBT_baseline_composit1.1.rda")
mu = as.data.frame(CBT_baseline_composit1.1[[3]][1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_areas(mu, pars = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")

CBT_baseline_composit2 <- readRDS("fitted_data/CBT_baseline_composit2.rda")
CBT_baseline_composit_pars <- rstan::extract(CBT_baseline_composit2)
mu = as.data.frame(CBT_baseline_composit_pars[1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_areas(mu, pars = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")
cov = as.data.frame(CBT_baseline_composit_pars[9:12])
colnames(cov) = c(paste0(alpha, '_base'), paste0(alpha, '_vs'), 
                  paste0(alpha, '_gb'), paste0(alpha, '_vs_gb'))
mcmc_intervals(cov, pars = c(paste0(alpha, '_base'), paste0(alpha, '_vs'), 
                             paste0(alpha, '_gb'), paste0(alpha, '_vs_gb')),
               prob = 0.95, prob_outer = 0.99,
               point_est = "mean")+ xlab("Effects of STAI on decision making")
HDIofMCMC(cov$α_base)
HDIofMCMC(cov$α_gb)
HDIofMCMC(cov$α_vs)

CBT_followup_composit2 <- readRDS("fitted_data/CBT_followup_composit2_fit.rda")
CBT_followup_composit_pars <- rstan::extract(CBT_followup_composit2)
mu = as.data.frame(CBT_followup_composit_pars[1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_areas(mu, pars = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")
cov = as.data.frame(CBT_followup_composit_pars[9:12])
colnames(cov) = c(paste0(alpha, '_base'), paste0(alpha, '_vs'), 
                  paste0(alpha, '_gb'), paste0(alpha, '_vs_gb'))
mcmc_intervals(cov, pars = c(paste0(alpha, '_base'), paste0(alpha, '_vs'), 
                             paste0(alpha, '_gb'), paste0(alpha, '_vs_gb')),
               prob = 0.95, prob_outer = 0.99,
               point_est = "mean")+ xlab("Effects of AD on decision making")
HDIofMCMC(cov$α_base)
HDIofMCMC(cov$α_gb)
HDIofMCMC(cov$α_vs)




CBT_baseline = read.csv("clean_data/CBT_baseline_matched.csv")
CBT_followup = read.csv("clean_data/CBT_followup_matched.csv")


CBT_baseline_df = data.frame(subjID = unique(CBT_baseline$subjID),
                             A_base = colMeans(as.data.frame(CBT_baseline_composit2.1[[3]][15])),
                             A_vs = colMeans(as.data.frame(CBT_baseline_composit2.1[[3]][16])),
                             treatment = 'baseline')
CBT_followup_df = data.frame(subjID = unique(CBT_followup$subjID),
                             A_base = colMeans(as.data.frame(CBT_followup_composit2.1[[3]][15])),
                             A_vs = colMeans(as.data.frame(CBT_followup_composit2.1[[3]][16])),
                             treatment = 'followup')
CBT_followup_df['subjID'] <- sub("C4", "C1", CBT_followup_df$subjID)

intersectIDs = intersect(unique(CBT_baseline_df$subjID),
                         unique(CBT_followup_df$subjID))
CBT_baseline_df = subset(CBT_baseline_df, 
                         CBT_baseline_df$subjID %in% intersectIDs)
CBT_followup_df = subset(CBT_followup_df, 
                         CBT_followup_df$subjID %in% intersectIDs)
CBT_A_vs = rbind(CBT_baseline_df, CBT_followup_df)


ggplot(CBT_A_vs,aes(x=treatment,y=A_base,fill=treatment,col=treatment))+
  geom_point(aes(color=treatment),size=2) +
  geom_line(aes(group = subjID),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(A_vs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())


CBT_followup_composit2.1 <- readRDS("fitted_data/CBT_followup_composit2.1.rda")
mu = as.data.frame(CBT_followup_composit2.1[[3]][1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_areas(mu, pars = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")
cov = as.data.frame(CBT_followup_composit2.1[[3]][9:14])
colnames(cov) = c(paste0(alpha, '_base'), paste0(alpha, '_vs'), 
                  paste0(alpha, '_gb'), paste0(alpha, '_vs_gb'), beta, gamma)
mcmc_intervals(cov, pars = c(paste0(alpha, '_base'), paste0(alpha, '_vs'), 
                             paste0(alpha, '_gb'), paste0(alpha, '_vs_gb'), beta, gamma),
               prob = 0.95, prob_outer = 0.99,
               point_est = "mean")+ xlab("Effects of STAI on decision making")
HDIofMCMC(cov$α_base)
HDIofMCMC(cov$α_gb)
HDIofMCMC(cov$α_vs)














##### plot group-level posterior distribution (especially the learning rates) #######
learning_rate = as.data.frame(schedule1_multi_block_pars[1])
learning_rate['diff'] = learning_rate$mu_A.2 - learning_rate$mu_A.1

mcmc_areas(learning_rate, pars = c("mu_A.1", "mu_A.2", "diff"),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")

mcmc_scatter(learning_rate, pars = c("mu_A.1", "mu_A.2"), 
             size = 1.5, alpha = 0.5)

if (requireNamespace("hexbin", quietly = FALSE)) {
  mcmc_hex(learning_rate, pars = c("mu_A.1", "mu_A.2"))
}
# Methods of ploting HDI posterior from HBayesDM
# A_HDI = HDIofMCMC(learning_rate$diff)
# ggplot(learning_rate, aes(x = diff)) +
#   geom_histogram(aes(), colour = "black", fill = "grey", bins = 30) +
#   geom_segment(aes(x = A_HDI[1], y = 0, xend = A_HDI[2], yend = 0), size = 1.5, colour = "red") + 
#   theme_minimal(base_size = 15) +
#   ggtitle('mu_A_base') +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
#   theme(axis.title.y = ggplot2::element_blank(),
#         axis.title.x = ggplot2::element_blank()
#         # plot.title = ggplot2::element_blank()
#   ) 

#### posterior distribution for all parameters ####
schedule1_pos = plot_posterior(groups = c("stable", "volatile"), 
                               all_fits = schedule1_multi_block_pars,
                               color_scheme = c("stable" = "#ffbdbc", "volatile" = "#2d95db"),
                               name = "group-level multi-block posterior distribution",
                               model_type = 'multi_block')

##### plot multivariate normal distribution individual parameters #####
plot_individual_change_multi_block1(schedule1_fit = schedule1_multi_block$fit, 
                                    "individual multi-block test")

# correlation examine of the individual means
cor(colMeans(schedule1_multi_block$fit$A[,,1]), colMeans(schedule1_multi_block$fit$A[,,2])) #0.68
A_stable = c(colMeans(schedule1_multi_block$fit$A[,,1]))
A_volatile = c(colMeans(schedule1_multi_block$fit$A[,,2]))
plot(A_stable, A_volatile, xlim=c(0, 0.4), ylim=c(0,0.4))
abline(a=0,b=1)


##### plot composit model parameters #####

rat1_clean <- readRDS("fitted_data/rat1_clean2.rda")
rat1_clean_pars <- rat1_clean[[3]][1]

mu_pr = as.data.frame(rat1_clean_pars[['mu_pr']])
colnames(mu_pr) = c("mu_A_base", "mu_A_vs", "mu_A_gb", "mu_A_vs_gb",
                    "mu_beta_base", "mu_beta_vs", "mu_beta_gb", "mu_beta_vs_gb",
                    "mu_gamma_base", "mu_gamma_vs", "mu_gamma_gb", "mu_gamma_vs_gb")

mcmc_areas(mu_pr, pars = c("A_base_cov"),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")



rat1_clean <- readRDS("fitted_data/rat1_clean2.rda")
rat1_clean_A_base_cov <- rat1_clean[[3]][['beta_vs_cov']]
colnames(rat1_clean_A_base_cov) = c("KPPSscore", "pdqtotal", "updrsPART1TOTAL")
mcmc_areas(rat1_clean_A_base_cov, pars = c("KPPSscore", "pdqtotal", "updrsPART1TOTAL"),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")


mu_pr = as.data.frame(rat1_clean_pars[['mu_pr']])
colnames(mu_pr) = c("mu_A_base", "mu_A_vs", "mu_A_gb", "mu_A_vs_gb",
                    "mu_beta_base", "mu_beta_vs", "mu_beta_gb", "mu_beta_vs_gb",
                    "mu_gamma_base", "mu_gamma_vs", "mu_gamma_gb", "mu_gamma_vs_gb")

mcmc_areas(mu_pr, pars = c("mu_A_base", "mu_A_vs", 'mu_A_gb', "mu_A_vs_gb"),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean")




