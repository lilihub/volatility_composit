## Functions for fitting and testing the very initial model in Browning's paper

fit_model <- function(raw_data_file, model_name, save_name){
  raw_data <- data.table::fread(file = paste0("clean_data/", raw_data_file, ".csv"),
                                header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE,
                                stringsAsFactors = TRUE, logical01 = FALSE)
  ev <- as.vector(t(read.csv("fitted_data/chris_model2_ev/control_baseline_schedule2_volatile_ev.csv")))
  # raw_data <- raw_data %>% drop_na()
  data_list <- frame2list(raw_data, ev)
  # data_list <- frame2list(raw_data)
  fit <- stan(
    file = paste0("stan_files/", model_name),
    data = data_list,
    iter = 4000,
    warmup = 1000,
    chains = 4
  )

  saveRDS(fit, file = paste0("fitted_data/", save_name, "_fit.rda"))
  # fit_extract <- rstan::extract(fit)
  # output = list(raw_data = raw_data, data_list = data_list, fit = fit_extract)
  # saveRDS(output, file = paste0("fitted_data/", raw_data_file, ".rda"))
  return(fit)
}

frame2list <- function(df, ev){
  payscale = 100
  DT_trials <- df[, .N, by = "subjID"]
  subjs     <- DT_trials$subjID
  n_subj    <- length(subjs)
  t_subjs   <- DT_trials$N
  t_max     <- max(t_subjs)
  # initV_ev  <- rep(0.5, n_subj)
  initV_ev <- ev

  # Initialize (model-specific) data arrays
  choice  <- array(-1, c(n_subj, t_max))   ###recycle -1 array n_subj rows and t_max columns
  outcome_blue <- array( -1, c(n_subj, t_max))
  outcome <- array(-1, c(n_subj, t_max))
  amount_blue <- array(0, c(n_subj, t_max))
  amount_orange <- array(0, c(n_subj, t_max))
  choiceB  <- array(-1, c(n_subj, t_max))   ###recycle -1 array n_subj rows and t_max columns

  # Write from raw_data to the data arrays
  for (i in 1:n_subj) {
    subj <- subjs[i]
    t <- t_subjs[i]
    DT_subj <- df[df$subjID == subj]

    choice[i, 1:t]  <- DT_subj$choice
    choiceB[i, 1:t] <- DT_subj$choiceB
    outcome_blue[i, 1:t] <- DT_subj$outcome_blue
    outcome[i, 1:t] <- DT_subj$outcome
    amount_blue[i, 1:t] <- DT_subj$amount_blue
    amount_orange[i, 1:t] <- DT_subj$amount_orange
  }
  # Wrap into a list for Stan
  data_list <- list(
    N       = n_subj,
    T       = t_max,
    Tsubj   = t_subjs,
    choice  = choice,
    choiceB = choiceB,
    outcome_blue = outcome_blue,
    outcome = outcome,
    amount_blue = amount_blue / payscale,
    amount_orange = amount_orange / payscale,
    initV_ev = initV_ev
  )
  return(data_list)
}
