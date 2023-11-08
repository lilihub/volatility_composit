rm(list=ls())
library(rstan)
library(loo)
extract_ic <- function(model_data = NULL) {
  IC <- list()
  IC$LOOIC = loo(model_data, save_psis = TRUE)
  return(IC)
}

control_baseline_composit0 <- readRDS("fitted_data/chris_model2_composit0/control_baseline_fit.rda")
control_baseline_composit1 <- readRDS("fitted_data/chris_model2_composit1/control_baseline_fit.rda")
control_baseline_composit2 <- readRDS("fitted_data/chris_model2_composit2/control_baseline_fit.rda")
control_baseline_composit3 <- readRDS("fitted_data/chris_model2_composit3/control_baseline_fit.rda")
control_baseline_composit4 <- readRDS("fitted_data/chris_model2_composit4/control_baseline_fit.rda")


loo1 = loo(control_baseline_composit3)
plot(
  loo1,
  diagnostic = c("k", "n_eff"),
  label_points = FALSE,
  main = "PSIS diagnostic plot"
)

modelTable = data.frame(Model = NULL, LOOIC = NULL)
modelList = list(control_baseline_composit0 = control_baseline_composit0, 
                 control_baseline_composit1 = control_baseline_composit1,
                 control_baseline_composit2 = control_baseline_composit2,
                 control_baseline_composit3 = control_baseline_composit3,
                 control_baseline_composit4 = control_baseline_composit4)

for (i in 1:length(modelList)) {
  modelTable[i, "Model"] = names(modelList)[i]
  modelTable[i, "LOOIC"] = extract_ic(modelList[[i]])$LOOIC$estimates[3,1]
}
modelTable


