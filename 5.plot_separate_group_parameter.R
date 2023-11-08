library(bayesplot)
##### read in the groups level parameters ######
chris_model2_control_baseline_group_par = read.csv("fitted_data/chris_model2/control_baseline_group_par.csv")
chris_model2_ev_control_baseline_group_par = read.csv("fitted_data/chris_model2_ev/control_baseline_group_par.csv")
mike_baseline_control_baseline_group_par = read.csv("fitted_data/mike_baseline/control_baseline_group_par.csv")
mike_baseline_updated_control_baseline_group_par = read.csv("fitted_data/mike_baseline_updated/control_baseline_group_par.csv")
mike_baseline_updated_ev_control_baseline_group_par = read.csv("fitted_data/mike_baseline_updated_ev/control_baseline_group_par.csv")


###### chris model2 #####
mcmc_areas(chris_model2_control_baseline_group_par, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_control_baseline_A")+
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(chris_model2_control_baseline_group_par, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_control_baseline_gamma")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(chris_model2_control_baseline_group_par, pars = c('schedule1_beta_stable', 'schedule1_beta_volatile', 'schedule2_beta_stable', 'schedule2_beta_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_control_baseline_beta")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

###### chris model2_ev #####
mcmc_areas(chris_model2_ev_control_baseline_group_par, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_ev_control_baseline_A")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(chris_model2_ev_control_baseline_group_par, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_ev_control_baseline_gamma")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(chris_model2_ev_control_baseline_group_par, pars = c('schedule1_beta_stable', 'schedule1_beta_volatile', 'schedule2_beta_stable', 'schedule2_beta_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "chris_model2_ev_control_baseline_beta")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


##### mike_baseline_updated #####
mcmc_areas(mike_baseline_updated_control_baseline_group_par, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_control_baseline_A")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(mike_baseline_updated_control_baseline_group_par, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_control_baseline_gamma")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(mike_baseline_updated_control_baseline_group_par, pars = c('schedule1_beta_stable', 'schedule1_beta_volatile', 'schedule2_beta_stable', 'schedule2_beta_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_control_baseline_beta")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


##### mike_baseline_updated_ev #####
mcmc_areas(mike_baseline_updated_ev_control_baseline_group_par, pars = c('schedule1_A_stable', 'schedule1_A_volatile', 'schedule2_A_stable', 'schedule2_A_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_ev_control_baseline_A")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(mike_baseline_updated_ev_control_baseline_group_par, pars = c('schedule1_gamma_stable', 'schedule1_gamma_volatile', 'schedule2_gamma_stable', 'schedule2_gamma_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_ev_control_baseline_gamma")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
mcmc_areas(mike_baseline_updated_ev_control_baseline_group_par, pars = c('schedule1_beta_stable', 'schedule1_beta_volatile', 'schedule2_beta_stable', 'schedule2_beta_volatile'),
           prob = 0.95, prob_outer = 0.99,
           point_est = "mean") + labs(title = "mike_baseline_updated_ev_control_baseline_beta")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

