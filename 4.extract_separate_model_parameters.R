#### control group clean data
control_baseline_schedule1_stable = read.csv('clean_data/control_baseline_schedule1_stable_matched.csv')
control_baseline_schedule2_stable = read.csv('clean_data/control_baseline_schedule2_stable_matched.csv')
subjID1 = as.character(unique(control_baseline_schedule1_stable$subjID))
subjID2 = as.character(unique(control_baseline_schedule2_stable$subjID))


############################# mike_baseline_updated_ev fitted data ###############
######## control group
### read in fitted results
mike_baseline_updated_ev_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_stable_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule1_stable_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule1_stable)
mike_baseline_updated_ev_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule1_volatile)

mike_baseline_updated_ev_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_stable_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule2_stable_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule2_stable)
mike_baseline_updated_ev_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_volatile_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule2_volatile)

### extract Group-level parameters
mike_baseline_updated_ev_mu_schedule1_stable = as.data.frame(mike_baseline_updated_ev_control_baseline_schedule1_stable_pars[9:11])
colnames(mike_baseline_updated_ev_mu_schedule1_stable) = c('schedule1_A_stable', 'schedule1_beta_stable', 'schedule1_gamma_stable')
mike_baseline_updated_ev_mu_schedule1_volatile = as.data.frame(mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars[9:11])
colnames(mike_baseline_updated_ev_mu_schedule1_volatile) = c('schedule1_A_volatile', 'schedule1_beta_volatile', 'schedule1_gamma_volatile')
mike_baseline_updated_ev_mu_schedule2_stable = as.data.frame(mike_baseline_updated_ev_control_baseline_schedule2_stable_pars[9:11])
colnames(mike_baseline_updated_ev_mu_schedule2_stable) = c('schedule2_A_stable', 'schedule2_beta_stable', 'schedule2_gamma_stable')
mike_baseline_updated_ev_mu_schedule2_volatile = as.data.frame(mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars[9:11])
colnames(mike_baseline_updated_ev_mu_schedule2_volatile) = c('schedule2_A_volatile', 'schedule2_beta_volatile', 'schedule2_gamma_volatile')


mike_baseline_updated_ev_mu = cbind(mike_baseline_updated_ev_mu_schedule1_stable, mike_baseline_updated_ev_mu_schedule1_volatile,
                                 mike_baseline_updated_ev_mu_schedule2_stable, mike_baseline_updated_ev_mu_schedule2_volatile)
write.csv(mike_baseline_updated_ev_mu, "fitted_data/mike_baseline_updated_ev/control_baseline_group_par.csv", row.names = FALSE)

## extract Individual-level parameters 
mike_baseline_updated_ev_control_baseline_par = data.frame(subjID = c(subjID1, subjID2),
                                                        stable_A = c(colMeans(mike_baseline_updated_ev_control_baseline_schedule1_stable_pars[['A']]),
                                                                     colMeans(mike_baseline_updated_ev_control_baseline_schedule2_stable_pars[['A']])),
                                                        volatile_A = c(colMeans(mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars[['A']]),
                                                                       colMeans(mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars[['A']])),
                                                        stable_beta = c(colMeans(mike_baseline_updated_ev_control_baseline_schedule1_stable_pars[['beta']]), 
                                                                        colMeans(mike_baseline_updated_ev_control_baseline_schedule2_stable_pars[['beta']])),
                                                        volatile_beta = c(colMeans(mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars[['beta']]), 
                                                                          colMeans(mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars[['beta']])),
                                                        stable_gamma = c(colMeans(mike_baseline_updated_ev_control_baseline_schedule1_stable_pars[['gamma']]), 
                                                                         colMeans(mike_baseline_updated_ev_control_baseline_schedule2_stable_pars[['gamma']])),
                                                        volatile_gamma = c(colMeans(mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars[['gamma']]), 
                                                                           colMeans(mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars[['gamma']])),
                                                        schedule = c(rep(1, time = length(subjID1)), rep(2, time=length(subjID2))))
write.csv(mike_baseline_updated_ev_control_baseline_par, "fitted_data/mike_baseline_updated_ev/control_baseline_individual_par.csv", row.names = FALSE)     

############################# chris_model2_ev fitted data ###############
######## control group
### read in fitted results
chris_model2_ev_control_baseline_schedule1_stable <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule1_stable_fit.rda")
chris_model2_ev_control_baseline_schedule1_stable_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule1_stable)
chris_model2_ev_control_baseline_schedule1_volatile <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule1_volatile_fit.rda")
chris_model2_ev_control_baseline_schedule1_volatile_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule1_volatile)

chris_model2_ev_control_baseline_schedule2_stable <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule2_stable_fit.rda")
chris_model2_ev_control_baseline_schedule2_stable_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule2_stable)
chris_model2_ev_control_baseline_schedule2_volatile <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule2_volatile_fit.rda")
chris_model2_ev_control_baseline_schedule2_volatile_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule2_volatile)

## extract Group-level parameters
chris_model2_ev_mu_schedule1_stable = as.data.frame(chris_model2_ev_control_baseline_schedule1_stable_pars[9:11])
colnames(chris_model2_ev_mu_schedule1_stable) = c('schedule1_A_stable', 'schedule1_beta_stable', 'schedule1_gamma_stable')
chris_model2_ev_mu_schedule1_volatile = as.data.frame(chris_model2_ev_control_baseline_schedule1_volatile_pars[9:11])
colnames(chris_model2_ev_mu_schedule1_volatile) = c('schedule1_A_volatile', 'schedule1_beta_volatile', 'schedule1_gamma_volatile')
chris_model2_ev_mu_schedule2_stable = as.data.frame(chris_model2_ev_control_baseline_schedule2_stable_pars[9:11])
colnames(chris_model2_ev_mu_schedule2_stable) = c('schedule2_A_stable', 'schedule2_beta_stable', 'schedule2_gamma_stable')
chris_model2_ev_mu_schedule2_volatile = as.data.frame(chris_model2_ev_control_baseline_schedule2_volatile_pars[9:11])
colnames(chris_model2_ev_mu_schedule2_volatile) = c('schedule2_A_volatile', 'schedule2_beta_volatile', 'schedule2_gamma_volatile')

chris_model2_ev_mu = cbind(chris_model2_ev_mu_schedule1_stable, chris_model2_ev_mu_schedule1_volatile,
                        chris_model2_ev_mu_schedule2_stable, chris_model2_ev_mu_schedule2_volatile)
write.csv(chris_model2_ev_mu, "fitted_data/chris_model2_ev/control_baseline_group_par.csv", row.names = FALSE)

## extract Individual-level parameters 
chris_model2_ev_control_baseline_par = data.frame(subjID = c(subjID1, subjID2),
                                               stable_A = c(colMeans(chris_model2_ev_control_baseline_schedule1_stable_pars[['A']]),
                                                            colMeans(chris_model2_ev_control_baseline_schedule2_stable_pars[['A']])),
                                               volatile_A = c(colMeans(chris_model2_ev_control_baseline_schedule1_volatile_pars[['A']]),
                                                              colMeans(chris_model2_ev_control_baseline_schedule2_volatile_pars[['A']])),
                                               stable_beta = c(colMeans(chris_model2_ev_control_baseline_schedule1_stable_pars[['beta']]),
                                                               colMeans(chris_model2_ev_control_baseline_schedule2_stable_pars[['beta']])),
                                               volatile_beta = c(colMeans(chris_model2_ev_control_baseline_schedule1_volatile_pars[['beta']]),
                                                                 colMeans(chris_model2_ev_control_baseline_schedule2_volatile_pars[['beta']])),
                                               stable_gamma = c(colMeans(chris_model2_ev_control_baseline_schedule1_stable_pars[['gamma']]),
                                                                colMeans(chris_model2_ev_control_baseline_schedule2_stable_pars[['gamma']])),
                                               volatile_gamma = c(colMeans(chris_model2_ev_control_baseline_schedule1_volatile_pars[['gamma']]),
                                                                  colMeans(chris_model2_ev_control_baseline_schedule2_volatile_pars[['gamma']])),
                                               schedule = c(rep(1, time = length(subjID1)), rep(2, time=length(subjID2))))
write.csv(chris_model2_ev_control_baseline_par, "fitted_data/chris_model2_ev/control_baseline_individual_par.csv", row.names = FALSE)



############################# chris_model2 fitted data ###############
######## control group
### read in fitted results
chris_model2_control_baseline_schedule1_stable <- readRDS("fitted_data/chris_model2/control_baseline_schedule1_stable_fit.rda")
chris_model2_control_baseline_schedule1_stable_pars <- rstan::extract(chris_model2_control_baseline_schedule1_stable)
chris_model2_control_baseline_schedule1_volatile <- readRDS("fitted_data/chris_model2/control_baseline_schedule1_volatile_fit.rda")
chris_model2_control_baseline_schedule1_volatile_pars <- rstan::extract(chris_model2_control_baseline_schedule1_volatile)

chris_model2_control_baseline_schedule2_stable <- readRDS("fitted_data/chris_model2/control_baseline_schedule2_stable_fit.rda")
chris_model2_control_baseline_schedule2_stable_pars <- rstan::extract(chris_model2_control_baseline_schedule2_stable)
chris_model2_control_baseline_schedule2_volatile <- readRDS("fitted_data/chris_model2/control_baseline_schedule2_volatile_fit.rda")
chris_model2_control_baseline_schedule2_volatile_pars <- rstan::extract(chris_model2_control_baseline_schedule2_volatile)

## extract Group-level parameters
chris_model2_mu_schedule1_stable = as.data.frame(chris_model2_control_baseline_schedule1_stable_pars[9:11])
colnames(chris_model2_mu_schedule1_stable) = c('schedule1_A_stable', 'schedule1_beta_stable', 'schedule1_gamma_stable')
chris_model2_mu_schedule1_volatile = as.data.frame(chris_model2_control_baseline_schedule1_volatile_pars[9:11])
colnames(chris_model2_mu_schedule1_volatile) = c('schedule1_A_volatile', 'schedule1_beta_volatile', 'schedule1_gamma_volatile')
chris_model2_mu_schedule2_stable = as.data.frame(chris_model2_control_baseline_schedule2_stable_pars[9:11])
colnames(chris_model2_mu_schedule2_stable) = c('schedule2_A_stable', 'schedule2_beta_stable', 'schedule2_gamma_stable')
chris_model2_mu_schedule2_volatile = as.data.frame(chris_model2_control_baseline_schedule2_volatile_pars[9:11])
colnames(chris_model2_mu_schedule2_volatile) = c('schedule2_A_volatile', 'schedule2_beta_volatile', 'schedule2_gamma_volatile')

chris_model2_mu = cbind(chris_model2_mu_schedule1_stable, chris_model2_mu_schedule1_volatile,
                        chris_model2_mu_schedule2_stable, chris_model2_mu_schedule2_volatile)
write.csv(chris_model2_mu, "fitted_data/chris_model2/control_baseline_group_par.csv", row.names = FALSE)


## extract Individual-level parameters 
chris_model2_control_baseline_par = data.frame(subjID = c(subjID1, subjID2),
                                               stable_A = c(colMeans(chris_model2_control_baseline_schedule1_stable_pars[['A']]),
                                                            colMeans(chris_model2_control_baseline_schedule2_stable_pars[['A']])),
                                               volatile_A = c(colMeans(chris_model2_control_baseline_schedule1_volatile_pars[['A']]),
                                                              colMeans(chris_model2_control_baseline_schedule2_volatile_pars[['A']])),
                                               stable_beta = c(colMeans(chris_model2_control_baseline_schedule1_stable_pars[['beta']]),
                                                               colMeans(chris_model2_control_baseline_schedule2_stable_pars[['beta']])),
                                               volatile_beta = c(colMeans(chris_model2_control_baseline_schedule1_volatile_pars[['beta']]),
                                                                 colMeans(chris_model2_control_baseline_schedule2_volatile_pars[['beta']])),
                                               stable_gamma = c(colMeans(chris_model2_control_baseline_schedule1_stable_pars[['gamma']]),
                                                                colMeans(chris_model2_control_baseline_schedule2_stable_pars[['gamma']])),
                                               volatile_gamma = c(colMeans(chris_model2_control_baseline_schedule1_volatile_pars[['gamma']]),
                                                                  colMeans(chris_model2_control_baseline_schedule2_volatile_pars[['gamma']])),
                                               schedule = c(rep(1, time = length(subjID1)), rep(2, time=length(subjID2))))
write.csv(chris_model2_control_baseline_par, "fitted_data/chris_model2/control_baseline_individual_par.csv", row.names = FALSE)



############################# mike_baseline_updated fitted data ###############
######## control group
### read in fitted results
mike_baseline_updated_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule1_stable_fit.rda")
mike_baseline_updated_control_baseline_schedule1_stable_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule1_stable)
mike_baseline_updated_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_updated_control_baseline_schedule1_volatile_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule1_volatile)

mike_baseline_updated_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule2_stable_fit.rda")
mike_baseline_updated_control_baseline_schedule2_stable_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule2_stable)
mike_baseline_updated_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule2_volatile_fit.rda")
mike_baseline_updated_control_baseline_schedule2_volatile_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule2_volatile)

### extract Group-level parameters
mike_baseline_updated_mu_schedule1_stable = as.data.frame(mike_baseline_updated_control_baseline_schedule1_stable_pars[9:11])
colnames(mike_baseline_updated_mu_schedule1_stable) = c('schedule1_A_stable', 'schedule1_beta_stable', 'schedule1_gamma_stable')
mike_baseline_updated_mu_schedule1_volatile = as.data.frame(mike_baseline_updated_control_baseline_schedule1_volatile_pars[9:11])
colnames(mike_baseline_updated_mu_schedule1_volatile) = c('schedule1_A_volatile', 'schedule1_beta_volatile', 'schedule1_gamma_volatile')
mike_baseline_updated_mu_schedule2_stable = as.data.frame(mike_baseline_updated_control_baseline_schedule2_stable_pars[9:11])
colnames(mike_baseline_updated_mu_schedule2_stable) = c('schedule2_A_stable', 'schedule2_beta_stable', 'schedule2_gamma_stable')
mike_baseline_updated_mu_schedule2_volatile = as.data.frame(mike_baseline_updated_control_baseline_schedule2_volatile_pars[9:11])
colnames(mike_baseline_updated_mu_schedule2_volatile) = c('schedule2_A_volatile', 'schedule2_beta_volatile', 'schedule2_gamma_volatile')


mike_baseline_updated_mu = cbind(mike_baseline_updated_mu_schedule1_stable, mike_baseline_updated_mu_schedule1_volatile,
                         mike_baseline_updated_mu_schedule2_stable, mike_baseline_updated_mu_schedule2_volatile)
write.csv(mike_baseline_updated_mu, "fitted_data/mike_baseline_updated/control_baseline_group_par.csv", row.names = FALSE)

## extract Individual-level parameters 
mike_baseline_updated_control_baseline_par = data.frame(subjID = c(subjID1, subjID2),
                                                        stable_A = c(colMeans(mike_baseline_updated_control_baseline_schedule1_stable_pars[['A']]),
                                                                     colMeans(mike_baseline_updated_control_baseline_schedule2_stable_pars[['A']])),
                                                        volatile_A = c(colMeans(mike_baseline_updated_control_baseline_schedule1_volatile_pars[['A']]),
                                                                       colMeans(mike_baseline_updated_control_baseline_schedule2_volatile_pars[['A']])),
                                                        stable_beta = c(colMeans(mike_baseline_updated_control_baseline_schedule1_stable_pars[['beta']]), 
                                                                        colMeans(mike_baseline_updated_control_baseline_schedule2_stable_pars[['beta']])),
                                                        volatile_beta = c(colMeans(mike_baseline_updated_control_baseline_schedule1_volatile_pars[['beta']]), 
                                                                          colMeans(mike_baseline_updated_control_baseline_schedule2_volatile_pars[['beta']])),
                                                        stable_gamma = c(colMeans(mike_baseline_updated_control_baseline_schedule1_stable_pars[['gamma']]), 
                                                                         colMeans(mike_baseline_updated_control_baseline_schedule2_stable_pars[['gamma']])),
                                                        volatile_gamma = c(colMeans(mike_baseline_updated_control_baseline_schedule1_volatile_pars[['gamma']]), 
                                                                           colMeans(mike_baseline_updated_control_baseline_schedule2_volatile_pars[['gamma']])),
                                                        schedule = c(rep(1, time = length(subjID1)), rep(2, time=length(subjID2))))
write.csv(mike_baseline_updated_control_baseline_par, "fitted_data/mike_baseline_updated/control_baseline_individual_par.csv", row.names = FALSE)     



############################# mike_baseline fitted data ###############
######## control group
### read in fitted results
mike_baseline_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline/control_baseline_schedule1_stable_fit.rda")
mike_baseline_control_baseline_schedule1_stable_pars <- rstan::extract(mike_baseline_control_baseline_schedule1_stable)
mike_baseline_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_control_baseline_schedule1_volatile_pars <- rstan::extract(mike_baseline_control_baseline_schedule1_volatile)

mike_baseline_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline/control_baseline_schedule2_stable_fit.rda")
mike_baseline_control_baseline_schedule2_stable_pars <- rstan::extract(mike_baseline_control_baseline_schedule2_stable)
mike_baseline_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline/control_baseline_schedule2_volatile_fit.rda")
mike_baseline_control_baseline_schedule2_volatile_pars <- rstan::extract(mike_baseline_control_baseline_schedule2_volatile)

### extract Group-level parameters
mike_baseline_mu_schedule1_stable = as.data.frame(mike_baseline_control_baseline_schedule1_stable_pars[9:11])
colnames(mike_baseline_mu_schedule1_stable) = c('schedule1_A_stable', 'schedule1_beta_stable', 'schedule1_gamma_stable')
mike_baseline_mu_schedule1_volatile = as.data.frame(mike_baseline_control_baseline_schedule1_volatile_pars[9:11])
colnames(mike_baseline_mu_schedule1_volatile) = c('schedule1_A_volatile', 'schedule1_beta_volatile', 'schedule1_gamma_volatile')
mike_baseline_mu_schedule2_stable = as.data.frame(mike_baseline_control_baseline_schedule2_stable_pars[9:11])
colnames(mike_baseline_mu_schedule2_stable) = c('schedule2_A_stable', 'schedule2_beta_stable', 'schedule2_gamma_stable')
mike_baseline_mu_schedule2_volatile = as.data.frame(mike_baseline_control_baseline_schedule2_volatile_pars[9:11])
colnames(mike_baseline_mu_schedule2_volatile) = c('schedule2_A_volatile', 'schedule2_beta_volatile', 'schedule2_gamma_volatile')


mike_baseline_mu = cbind(mike_baseline_mu_schedule1_stable, mike_baseline_mu_schedule1_volatile, 
                         mike_baseline_mu_schedule2_stable, mike_baseline_mu_schedule2_volatile)
write.csv(mike_baseline_mu, "fitted_data/mike_baseline/control_baseline_group_par.csv", row.names = FALSE)

## extract Individual-level parameters 

mike_baseline_control_baseline_par = data.frame(subjID = c(subjID1, subjID2),
                                                stable_A = c(colMeans(mike_baseline_control_baseline_schedule1_stable_pars[['A']]),
                                                                     colMeans(mike_baseline_control_baseline_schedule2_stable_pars[['A']])),
                                                volatile_A = c(colMeans(mike_baseline_control_baseline_schedule1_volatile_pars[['A']]),
                                                               colMeans(mike_baseline_control_baseline_schedule2_volatile_pars[['A']])),
                                                stable_beta = c(colMeans(mike_baseline_control_baseline_schedule1_stable_pars[['beta']]), 
                                                                colMeans(mike_baseline_control_baseline_schedule2_stable_pars[['beta']])),
                                                volatile_beta = c(colMeans(mike_baseline_control_baseline_schedule1_volatile_pars[['beta']]), 
                                                                  colMeans(mike_baseline_control_baseline_schedule2_volatile_pars[['beta']])),
                                                stable_gamma = c(colMeans(mike_baseline_control_baseline_schedule1_stable_pars[['gamma']]), 
                                                                 colMeans(mike_baseline_control_baseline_schedule2_stable_pars[['gamma']])),
                                                volatile_gamma = c(colMeans(mike_baseline_control_baseline_schedule1_volatile_pars[['gamma']]), 
                                                                   colMeans(mike_baseline_control_baseline_schedule2_volatile_pars[['gamma']])),
                                                schedule = c(rep(1, time = length(subjID1)), rep(2, time=length(subjID2))))
write.csv(mike_baseline_control_baseline_par, "fitted_data/mike_baseline/control_baseline_individual_par.csv", row.names = FALSE)     



