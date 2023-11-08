rm(list=ls())
library(rstan)
library(loo)
extract_ic <- function(model_data = NULL) {
  IC <- list()
  IC$LOOIC = loo(model_data, save_psis = TRUE)
  return(IC)
}

chris_model2_control_baseline_schedule1_stable <- readRDS("fitted_data/chris_model2/control_baseline_schedule1_stable_fit.rda")
chris_model2_control_baseline_schedule1_volatile <- readRDS("fitted_data/chris_model2/control_baseline_schedule1_volatile_fit.rda")
chris_model2_control_baseline_schedule2_stable <- readRDS("fitted_data/chris_model2/control_baseline_schedule2_stable_fit.rda")
chris_model2_control_baseline_schedule2_volatile <- readRDS("fitted_data/chris_model2/control_baseline_schedule2_volatile_fit.rda")

chris_model2_ev_control_baseline_schedule1_stable <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule1_stable_fit.rda")
chris_model2_ev_control_baseline_schedule1_volatile <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule1_volatile_fit.rda")
chris_model2_ev_control_baseline_schedule2_stable <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule2_stable_fit.rda")
chris_model2_ev_control_baseline_schedule2_volatile <- readRDS("fitted_data/chris_model2_ev/control_baseline_schedule2_volatile_fit.rda")

mike_baseline_updated_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule1_stable_fit.rda")
mike_baseline_updated_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_updated_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule2_stable_fit.rda")
mike_baseline_updated_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline_updated/control_baseline_schedule2_volatile_fit.rda")

mike_baseline_updated_ev_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_stable_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_stable_fit.rda")
mike_baseline_updated_ev_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline_updated_ev/control_baseline_schedule2_volatile_fit.rda")


mike_baseline_control_baseline_schedule1_stable <- readRDS("fitted_data/mike_baseline/control_baseline_schedule1_stable_fit.rda")
mike_baseline_control_baseline_schedule1_volatile <- readRDS("fitted_data/mike_baseline/control_baseline_schedule1_volatile_fit.rda")
mike_baseline_control_baseline_schedule2_stable <- readRDS("fitted_data/mike_baseline/control_baseline_schedule2_stable_fit.rda")
mike_baseline_control_baseline_schedule2_volatile <- readRDS("fitted_data/mike_baseline/control_baseline_schedule2_volatile_fit.rda")


# n_chains = 4
# loo_value <- function(fit){
#   log_lik <- extract_log_lik(fit, parameter_name = "log_lik")
#   r_eff <- relative_eff(exp(log_lik), 
#                         chain_id = rep(1:n_chains, each = nrow(log_lik) / n_chains),
#                         cores = getOption("mc.cores", parallel::detectCores()))
#   loo <- loo(log_lik, r_eff = r_eff,
#       cores = getOption("mc.cores", parallel::detectCores()))
#   # loo <- loo::waic(log_lik)
#   return(loo)
# }


########## chris_model2
chris_model2_Table = data.frame(Model = NULL, LOOIC = NULL)
chris_model2_List = list(chris_model2_control_baseline_schedule1_stable = chris_model2_control_baseline_schedule1_stable, 
                         chris_model2_control_baseline_schedule1_volatile = chris_model2_control_baseline_schedule1_volatile,
                         chris_model2_control_baseline_schedule2_stable = chris_model2_control_baseline_schedule2_stable,
                         chris_model2_control_baseline_schedule2_volatile = chris_model2_control_baseline_schedule2_volatile)

for (i in 1:length(chris_model2_List)) {
  chris_model2_Table[i, "Model"] = names(chris_model2_List)[i]
  chris_model2_Table[i, "LOOIC"] = extract_ic(chris_model2_List[[i]])$LOOIC$estimates[3,1]
}
chris_model2_Table[5, "Model"] = "chris_model2_sum"
chris_model2_Table[5, "LOOIC"] = sum(chris_model2_Table$LOOIC[1:4])

########## chris_model2_ev
chris_model2_ev_Table = data.frame(Model = NULL, LOOIC = NULL)
chris_model2_ev_List = list(chris_model2_ev_control_baseline_schedule1_stable = chris_model2_ev_control_baseline_schedule1_stable, 
                         chris_model2_ev_control_baseline_schedule1_volatile = chris_model2_ev_control_baseline_schedule1_volatile,
                         chris_model2_ev_control_baseline_schedule2_stable = chris_model2_ev_control_baseline_schedule2_stable,
                         chris_model2_ev_control_baseline_schedule2_volatile = chris_model2_ev_control_baseline_schedule2_volatile)

for (i in 1:length(chris_model2_ev_List)) {
  chris_model2_ev_Table[i, "Model"] = names(chris_model2_ev_List)[i]
  chris_model2_ev_Table[i, "LOOIC"] = extract_ic(chris_model2_ev_List[[i]])$LOOIC$estimates[3,1]
}
chris_model2_ev_Table[5, "Model"] = "chris_model2_ev_sum"
chris_model2_ev_Table[5, "LOOIC"] = sum(chris_model2_ev_Table$LOOIC[1:4])


######## mike_baseline_updated
mike_baseline_updated_Table = data.frame(Model = NULL, LOOIC = NULL)
mike_baseline_updated_List = list(mike_baseline_updated_control_baseline_schedule1_stable = mike_baseline_updated_control_baseline_schedule1_stable, 
                                  mike_baseline_updated_control_baseline_schedule1_volatile = mike_baseline_updated_control_baseline_schedule1_volatile,
                                  mike_baseline_updated_control_baseline_schedule2_stable = mike_baseline_updated_control_baseline_schedule2_stable,
                                  mike_baseline_updated_control_baseline_schedule2_volatile = mike_baseline_updated_control_baseline_schedule2_volatile)

for (i in 1:length(mike_baseline_updated_List)) {
  mike_baseline_updated_Table[i, "Model"] = names(mike_baseline_updated_List)[i]
  mike_baseline_updated_Table[i, "LOOIC"] = extract_ic(mike_baseline_updated_List[[i]])$LOOIC$estimates[3,1]
}
mike_baseline_updated_Table[5, "Model"] = "mike_baseline_updated_sum"
mike_baseline_updated_Table[5, "LOOIC"] = sum(mike_baseline_updated_Table$LOOIC[1:4])

######## mike_baseline_updated_ev
mike_baseline_updated_ev_Table = data.frame(Model = NULL, LOOIC = NULL)
mike_baseline_updated_ev_List = list(mike_baseline_updated_ev_control_baseline_schedule1_stable = mike_baseline_updated_ev_control_baseline_schedule1_stable, 
                                  mike_baseline_updated_ev_control_baseline_schedule1_volatile = mike_baseline_updated_ev_control_baseline_schedule1_volatile,
                                  mike_baseline_updated_ev_control_baseline_schedule2_stable = mike_baseline_updated_ev_control_baseline_schedule2_stable,
                                  mike_baseline_updated_ev_control_baseline_schedule2_volatile = mike_baseline_updated_ev_control_baseline_schedule2_volatile)

for (i in 1:length(mike_baseline_updated_ev_List)) {
  mike_baseline_updated_ev_Table[i, "Model"] = names(mike_baseline_updated_ev_List)[i]
  mike_baseline_updated_ev_Table[i, "LOOIC"] = extract_ic(mike_baseline_updated_ev_List[[i]])$LOOIC$estimates[3,1]
}
mike_baseline_updated_ev_Table[5, "Model"] = "mike_baseline_updated_ev_sum"
mike_baseline_updated_ev_Table[5, "LOOIC"] = sum(mike_baseline_updated_ev_Table$LOOIC[1:4])


######## mike_baseline
mike_baseline_Table = data.frame(Model = NULL, LOOIC = NULL)
mike_baseline_List = list(mike_baseline_control_baseline_schedule1_stable = mike_baseline_control_baseline_schedule1_stable, 
                                  mike_baseline_control_baseline_schedule1_volatile = mike_baseline_control_baseline_schedule1_volatile,
                                  mike_baseline_control_baseline_schedule2_stable = mike_baseline_control_baseline_schedule2_stable,
                                  mike_baseline_control_baseline_schedule2_volatile = mike_baseline_control_baseline_schedule2_volatile)

for (i in 1:length(modelList)) {
  mike_baseline_Table[i, "Model"] = names(mike_baseline_List)[i]
  mike_baseline_Table[i, "LOOIC"] = extract_ic(mike_baseline_List[[i]])$LOOIC$estimates[3,1]
}
mike_baseline_Table[5, "Model"] = "mike_baseline_sum"
mike_baseline_Table[5, "LOOIC"] = sum(mike_baseline_Table$LOOIC[1:4])



#########################  convergence check ####################################
chris_model2_control_baseline_schedule1_stable_summary <- rstan::summary(chris_model2_control_baseline_schedule1_stable)
chris_model2_control_baseline_schedule1_stable_rhat <- data.frame(Rhat = chris_model2_control_baseline_schedule1_stable_summary[["summary"]][, "Rhat"])
hist(chris_model2_control_baseline_schedule1_stable_rhat$Rhat)

chris_model2_control_baseline_schedule1_volatile_summary <- rstan::summary(chris_model2_control_baseline_schedule1_volatile)
chris_model2_control_baseline_schedule1_volatile_rhat <- data.frame(Rhat = chris_model2_control_baseline_schedule1_volatile_summary[["summary"]][, "Rhat"])
hist(chris_model2_control_baseline_schedule1_volatile_rhat$Rhat)

chris_model2_control_baseline_schedule2_volatile_summary <- rstan::summary(chris_model2_control_baseline_schedule2_volatile)
chris_model2_control_baseline_schedule2_volatile_rhat <- data.frame(Rhat = chris_model2_control_baseline_schedule2_volatile_summary[["summary"]][, "Rhat"])
hist(chris_model2_control_baseline_schedule2_volatile_rhat$Rhat)

mike_baseline_updated_control_baseline_schedule1_volatile_summary <- rstan::summary(mike_baseline_updated_control_baseline_schedule1_volatile)
mike_baseline_updated_control_baseline_schedule1_volatile_rhat <- data.frame(Rhat = mike_baseline_updated_control_baseline_schedule1_volatile_summary[["summary"]][, "Rhat"])
hist(mike_baseline_updated_control_baseline_schedule1_volatile_rhat$Rhat)
