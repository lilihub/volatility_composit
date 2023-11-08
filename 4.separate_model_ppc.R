library(ggplot2)
library(cowplot)
library(ggpubr)
library(abind)
library(bayesplot)
library(rstan)
source("utils/fit_separate_functions.R")
source("utils/ppc_utils.R")

############## read in control_baseline raw data ################
control_baseline_schedule1_stable <- data.table::fread(file = "clean_data/control_baseline_schedule1_stable_matched.csv",
                                                       header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE,
                                                       stringsAsFactors = TRUE, logical01 = FALSE)
control_baseline_schedule1_volatile <- data.table::fread(file = "clean_data/control_baseline_schedule1_volatile_matched.csv",
                                                         header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE,
                                                         stringsAsFactors = TRUE, logical01 = FALSE)
control_baseline_schedule2_stable <- data.table::fread(file = "clean_data/control_baseline_schedule2_stable_matched.csv",
                                                       header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE,
                                                       stringsAsFactors = TRUE, logical01 = FALSE)
control_baseline_schedule2_volatile <- data.table::fread(file = "clean_data/control_baseline_schedule2_volatile_matched.csv",
                                                         header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE,
                                                         stringsAsFactors = TRUE, logical01 = FALSE)
control_baseline_schedule1_stable_list <- frame2list(control_baseline_schedule1_stable)
control_baseline_schedule1_volatile_list <- frame2list(control_baseline_schedule1_volatile)
control_baseline_schedule2_stable_list <- frame2list(control_baseline_schedule2_stable)
control_baseline_schedule2_volatile_list <- frame2list(control_baseline_schedule2_volatile)

################### read in chris_model2 fitted data##################
chris_model2_control_baseline_schedule1_stable <- readRDS("fitted_data/chris_model2/control_baseline_schedule1_stable_fit.rda")
chris_model2_control_baseline_schedule1_volatile <- readRDS("fitted_data/chris_model2/control_baseline_schedule1_volatile_fit.rda")
chris_model2_control_baseline_schedule2_stable <- readRDS("fitted_data/chris_model2/control_baseline_schedule2_stable_fit.rda")
chris_model2_control_baseline_schedule2_volatile <- readRDS("fitted_data/chris_model2/control_baseline_schedule2_volatile_fit.rda")

chris_model2_control_baseline_schedule1_stable_pars <- rstan::extract(chris_model2_control_baseline_schedule1_stable)
chris_model2_control_baseline_schedule1_volatile_pars <- rstan::extract(chris_model2_control_baseline_schedule1_volatile)
chris_model2_control_baseline_schedule2_stable_pars <- rstan::extract(chris_model2_control_baseline_schedule2_stable)
chris_model2_control_baseline_schedule2_volatile_pars <- rstan::extract(chris_model2_control_baseline_schedule2_volatile)


ppc_stable(fit_extract = chris_model2_control_baseline_schedule1_stable_pars, 
           list_data = control_baseline_schedule1_stable_list,
           schedule = 1,
           file_name = "chris_model2_control_baseline_schedule1_stable")

ppc_volatile(fit_extract = chris_model2_control_baseline_schedule1_volatile_pars, 
             list_data = control_baseline_schedule1_volatile_list,
             schedule =1,
             choice_code = 0,
             file_name = "chris_model2_control_baseline_schedule1_volatile")

ppc_stable(fit_extract = chris_model2_control_baseline_schedule2_stable_pars, 
           list_data = control_baseline_schedule2_stable_list,
           schedule = 2,
           file_name = "chris_model2_control_baseline_schedule2_stable")

ppc_volatile(fit_extract = chris_model2_control_baseline_schedule2_volatile_pars, 
             list_data = control_baseline_schedule2_volatile_list,
             schedule = 2,
             choice_code = 0,
             file_name = "chris_model2_control_baseline_schedule2_volatile")

################### read in chris_model2_ev fitted data##################
chris_model2_ev_control_baseline_schedule1_stable <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule1_stable_fit.rda")
chris_model2_ev_control_baseline_schedule1_volatile <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule1_volatile_fit.rda")
chris_model2_ev_control_baseline_schedule2_stable <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule2_stable_fit.rda")
chris_model2_ev_control_baseline_schedule2_volatile <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule2_volatile_fit.rda")

chris_model2_ev_control_baseline_schedule1_stable_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule1_stable)
chris_model2_ev_control_baseline_schedule1_volatile_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule1_volatile)
chris_model2_ev_control_baseline_schedule2_stable_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule2_stable)
chris_model2_ev_control_baseline_schedule2_volatile_pars <- rstan::extract(chris_model2_ev_control_baseline_schedule2_volatile)


ppc_stable(fit_extract = chris_model2_ev_control_baseline_schedule1_stable_pars, 
           list_data = control_baseline_schedule1_stable_list,
           schedule = 1,
           file_name = "chris_model2_ev_control_baseline_schedule1_stable")

ppc_volatile(fit_extract = chris_model2_ev_control_baseline_schedule1_volatile_pars, 
             list_data = control_baseline_schedule1_volatile_list,
             schedule =1,
             choice_code = 0,
             file_name = "chris_model2_ev_control_baseline_schedule1_volatile")

ppc_stable(fit_extract = chris_model2_ev_control_baseline_schedule2_stable_pars, 
           list_data = control_baseline_schedule2_stable_list,
           schedule = 2,
           file_name = "chris_model2_ev_control_baseline_schedule2_stable")

ppc_volatile(fit_extract = chris_model2_ev_control_baseline_schedule2_volatile_pars, 
             list_data = control_baseline_schedule2_volatile_list,
             schedule = 2,
             choice_code = 0,
             file_name = "chris_model2_ev_control_baseline_schedule2_volatile")

################### read in mike_baseline_updated fitted data##################
mike_baseline_updated_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule1_stable_fit.rda")
mike_baseline_updated_control_baseline_schedule1_stable_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule1_stable)
mike_baseline_updated_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_updated_control_baseline_schedule1_volatile_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule1_volatile)

mike_baseline_updated_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule2_stable_fit.rda")
mike_baseline_updated_control_baseline_schedule2_stable_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule2_stable)
mike_baseline_updated_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule2_volatile_fit.rda")
mike_baseline_updated_control_baseline_schedule2_volatile_pars <- rstan::extract(mike_baseline_updated_control_baseline_schedule2_volatile)

ppc_stable(fit_extract = mike_baseline_updated_control_baseline_schedule1_stable_pars, 
           list_data = control_baseline_schedule1_stable_list,
           schedule = 1,
           file_name = "mike_baseline_updated_control_baseline_schedule1_stable")

ppc_volatile(fit_extract = mike_baseline_updated_control_baseline_schedule1_volatile_pars, 
             list_data = control_baseline_schedule1_volatile_list,
             schedule =1,
             choice_code = 0,
             file_name = "mike_baseline_updated_control_baseline_schedule1_volatile")

ppc_stable(fit_extract = mike_baseline_updated_control_baseline_schedule2_stable_pars, 
           list_data = control_baseline_schedule2_stable_list,
           schedule = 2,
           file_name = "mike_baseline_updated_control_baseline_schedule2_stable")

ppc_volatile(fit_extract = mike_baseline_updated_control_baseline_schedule2_volatile_pars, 
             list_data = control_baseline_schedule2_volatile_list,
             schedule = 2,
             choice_code = 0,
             file_name = "mike_baseline_updated_control_baseline_schedule2_volatile")

################### read in mike_baseline_updated_Ev fitted data##################
mike_baseline_updated_ev_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_stable_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule1_stable_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule1_stable)
mike_baseline_updated_ev_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule1_volatile)

mike_baseline_updated_ev_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_stable_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule2_stable_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule2_stable)
mike_baseline_updated_ev_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_volatile_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars <- rstan::extract(mike_baseline_updated_ev_control_baseline_schedule2_volatile)

ppc_stable(fit_extract = mike_baseline_updated_ev_control_baseline_schedule1_stable_pars, 
           list_data = control_baseline_schedule1_stable_list,
           schedule = 1,
           file_name = "mike_baseline_updated_ev_control_baseline_schedule1_stable")

ppc_volatile(fit_extract = mike_baseline_updated_ev_control_baseline_schedule1_volatile_pars, 
             list_data = control_baseline_schedule1_volatile_list,
             schedule =1,
             choice_code = 0,
             file_name = "mike_baseline_updated_ev_control_baseline_schedule1_volatile")

ppc_stable(fit_extract = mike_baseline_updated_ev_control_baseline_schedule2_stable_pars, 
           list_data = control_baseline_schedule2_stable_list,
           schedule = 2,
           file_name = "mike_baseline_updated_ev_control_baseline_schedule2_stable")

ppc_volatile(fit_extract = mike_baseline_updated_ev_control_baseline_schedule2_volatile_pars, 
             list_data = control_baseline_schedule2_volatile_list,
             schedule = 2,
             choice_code = 0,
             file_name = "mike_baseline_updated_ev_control_baseline_schedule2_volatile")
