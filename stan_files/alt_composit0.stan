////////////////////////////////////
//
// Model #2 in Gagne's paper(2020) for the Aversive Learning Task
// note: estimates stable and volatile conditions simultaneously
// covariates for learning rate components
// written by Lili January 2023
//
////////////////////////////////////

data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N]; // for subjects with missing trials
  int<lower=-1, upper=2> choiceB[N, T];
  real outcome_blue[N, T];  // no lower and upper bounds
  real amount_blue[N, T];
  real amount_orange[N, T];
}
transformed data {
  real initV_ev;  // initial values for EV blue
  initV_ev = 0.5;
}
parameters {
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[3] mu_pr;
  vector<lower=0>[3] sigma;
  
  vector[N] A_base_pr;    // baseline learning rate
  vector[N] beta_pr; // baseline inverse temperature
  vector[N] gamma_pr; // baseline risk preference
}

transformed parameters {
  // individual parameters
    // Subject-level raw parameters (for Matt trick)
  vector[N] A_base;    // baseline learning rate
  vector[N] beta; // baseline inverse temperature
  vector[N] gamma; // baseline risk preference
 
  for(i in 1:N){
    A_base[i] = inv_logit(mu_pr[1] +  sigma[1]*A_base_pr[i]);
    beta[i] = inv_logit(mu_pr[2] + sigma[2]*beta_pr[i])*10;
    gamma[i] = inv_logit(mu_pr[3] + sigma[3]*gamma_pr[i]);
  }
}
model {
  // Hyperparameters
  mu_pr  ~ normal(0, 1);
  sigma ~ normal(0, 0.2);
  
  A_base_pr ~ normal(0, 1);
  beta_pr ~ normal(0, 1);
  gamma_pr ~ normal(0, 1);
 
   // subject loop and trial loop
  for (i in 1:N) {
    real ev; // expected value
    real gvalue;
    real PE;      // prediction error
    ev = initV_ev;
    for (t in 1:Tsubj[i]) {
      gvalue = gamma[i] * (ev - (1 - ev)) + (1 - gamma[i]) * (amount_blue[i,t] - amount_orange[i, t]); // gvalue of the blue choice
      // compute action probabilities (consider to replace categorical_logit to bernoulli_logit, 
      // but it requires the input to be one dimension and choice should be coded as 1 (e.g. the blue one was chosen)
      // and 0 (the blue one was not chosen))
      choiceB[i, t] ~ bernoulli_logit((-beta[i]) * gvalue);
      // prediction error of the blue choice
      PE = outcome_blue[i, t] - ev;
      // value updating (learning) only updating the ev of the blue choice
      ev += A_base[i] * PE;
    }
  }
}

generated quantities{
  // For log likelihood calculation
  real log_lik[N];
  // For posterior predictive check
  real y_pred[N, T];
  
  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }
  
  { // local section, this saves time and space
    for (i in 1:N) {
      real ev; // expected value
      real PE;      // prediction error
      real gvalue;
      // Initialize values
      ev = initV_ev;
      //gvalue = initV_gvalue;

      log_lik[i] = 0;

      for (t in 1:Tsubj[i]) {
        gvalue = gamma[i] * (ev - (1 - ev)) + (1 - gamma[i]) * (amount_blue[i, t] - amount_orange[i, t]); // gvalue of the blue choice
        // compute log likelihood of current trial
        log_lik[i] += bernoulli_logit_lpmf(choiceB[i, t] | (-beta[i]) * gvalue);
        // generate posterior prediction for current trial
        y_pred[i, t] = bernoulli_logit_rng((-beta[i]) * gvalue);
        // prediction error
        PE = outcome_blue[i, t] - ev;
        // value updating (learning)
        ev += A_base[i] * PE;
      }
    }
  }
}


