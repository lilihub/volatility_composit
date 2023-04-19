fit_composit_model <- function(choice_file, clinical_file, nC, cov_name, model_name, save_name){
  raw_data <- data.table::fread(file = paste0("clean_data/", choice_file, ".csv"),
                                header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE, 
                                stringsAsFactors = TRUE, logical01 = FALSE)
  raw_data <- raw_data %>% drop_na()
  
  clinical_raw <- read.csv(paste0("clean_data/", clinical_file, ".csv"), stringsAsFactors = TRUE)
  # clinical_select = clinical_raw[, cov]
  data_list <- frame2list(raw_data, clinical_raw, nC, cov_name)
  
  fit <- stan(
    file = paste0("stan_files/", model_name),
    data = data_list,
    warmup = 1000,
    iter = 3000,
    chains = 4
  )
  
  saveRDS(fit, file = paste0("fitted_data/", save_name, "_fit.rda"))
  # fit_extract <- rstan::extract(fit)
  # output = list(raw_data = raw_data, list_data = data_list, fit = fit_extract)
  # saveRDS(output, file = paste0("fitted_data/", save_name, ".rda"))
  return(fit)
}

frame2list <- function(df, df1, nC, cov_name){
  payscale = 100
  DT_trials <- df[, .N, by = "subjID"]
  subjs     <- DT_trials$subjID
  n_subj    <- length(subjs)
  t_subjs   <- DT_trials$N
  t_max     <- max(t_subjs)
  
  # Initialize (model-specific) data arrays
  choice  <- array(-1, c(n_subj, t_max))   ###recycle -1 array n_subj rows and t_max columns
  choiceB  <- array(-1, c(n_subj, t_max))   ###recycle -1 array n_subj rows and t_max columns
  outcome_blue <- array( -1, c(n_subj, t_max))
  outcome_blue1 <- array( -1, c(n_subj, t_max))
  outcome <- array(-1, c(n_subj, t_max))
  amount_blue <- array(0, c(n_subj, t_max))
  amount_orange <- array(0, c(n_subj, t_max))
  status <- array(0, c(n_subj, t_max))
  out <- array(0, c(n_subj, t_max))
  if(nC>0){
    cov <- array(0, c(n_subj, nC))
  }
  
  # Write from raw_data to the data arrays
  for (i in 1:n_subj) {
    subj <- as.character(subjs[i])
    t <- t_subjs[i]
    DT_subj <- df[df$subjID == subj]
    
    choice[i, 1:t]  <- DT_subj$choice
    choiceB[i, 1:t] <- DT_subj$choiceB
    outcome_blue[i, 1:t] <- DT_subj$outcome_blue
    outcome_blue1[i, 1:t] <- DT_subj$outcome_blue1
    outcome[i, 1:t] <- DT_subj$outcome
    amount_blue[i, 1:t] <- DT_subj$amount_blue
    amount_orange[i, 1:t] <- DT_subj$amount_orange
    status[i, 1:t] <- DT_subj$status
    out[i, 1:t] <- DT_subj$out
    if(nC > 0){
      cov[i,1:nC] <- as.vector(t(df1[df1$subjID == subj, cov_name]))
    }
  }
  # Wrap into a list for Stan
  data_list <- list(
    N       = n_subj,
    T       = t_max,
    Tsubj   = t_subjs,
    choice  = choice,
    choiceB = choiceB,
    outcome_blue = outcome_blue,
    outcome_blue1 = outcome_blue1,
    outcome = outcome,
    amount_blue = amount_blue / payscale,
    amount_orange = amount_orange / payscale,
    status = status,
    out = out
  )
  if(nC>0){
    data_list[['nC']] = nC
    data_list[['cov']] = cov
  }
  return(data_list)
}
