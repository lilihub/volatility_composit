////////////////////////////////////
//
// the basic HB model in Browning's paper(2015) for the Aversive Learning Task
// note: this script estimates each group and condition separately without 
// combining any covariants
// written by Lili July 2021
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
  // Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;    // learning rate
  vector[N] beta_pr; // inverse temperature
  vector[N] gamma_pr; // risk preference
}

transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] A;
  vector<lower=0, upper=10>[N] beta;
  vector<lower=0, upper=10>[N] gamma;
  for (i in 1:N) {
    A[i]   = Phi_approx(mu_pr[1] + sigma[1]  * A_pr[i]);
    beta[i] = Phi_approx(mu_pr[2] + sigma[2] * beta_pr[i]) * 10;
    gamma[i] = Phi_approx(mu_pr[3] + sigma[3] * gamma_pr[i]);
  }
}
model {
  // Hyperparameters
  mu_pr  ~ normal(0, 1);
  sigma ~ normal(0, 0.2);
  // individual parameters
  A_pr ~ normal(0, 1.0);
  beta_pr ~ normal(0, 1.0);
  gamma_pr ~ normal(0, 1.0);
  // subject loop and trial loop
  for (i in 1:N) {
    real ev; // expected value
    real gvalue;
    real PE;      // prediction error
    ev = initV_ev;
    for (t in 1:Tsubj[i]) {
      
      gvalue = gamma[i] * (ev - (1 - ev)) + (1 - gamma[i]) * (amount_blue[i,t] - amount_orange[i, t]);
      // compute action probabilities
      choiceB[i, t] ~ bernoulli_logit((-beta[i]) * gvalue);
      // prediction error of the blue choice
      PE = outcome_blue[i, t] - ev;
      // value updating (learning) only updating the ev of the blue choice
      ev += A[i] * PE;
    }
  }
}

generated quantities{
  real<lower=0, upper=1> mu_A; // the mean learning rate of the population
  real<lower=0, upper=10> mu_beta; // the mean inverse temperature of the population
  real<lower=0, upper=10> mu_gamma; // the mean risk preference of the population
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
  mu_A = Phi_approx(mu_pr[1]);
  mu_beta = Phi_approx(mu_pr[2]) * 10;
  mu_gamma = Phi_approx(mu_pr[3]);
 
  { // local section, this saves time and space
    for (i in 1:N) {
      real ev; // expected value
      real gvalue;
      real PE;      // prediction error

      // Initialize values
      ev = initV_ev;
      //gvalue = initV_gvalue;

      log_lik[i] = 0;

      for (t in 1:Tsubj[i]) {
        
        gvalue = gamma[i] * (ev - (1 - ev)) + (1 - gamma[i]) * (amount_blue[i,t] - amount_orange[i, t]); // gvalue of the blue choice

        // compute log likelihood of current trial
        log_lik[i] += bernoulli_logit_lpmf(choiceB[i, t] | (-beta[i]) * gvalue);
        // generate posterior prediction for current trial
        y_pred[i, t] = bernoulli_logit_rng((-beta[i]) * gvalue);
         // prediction error
        PE = outcome_blue[i, t] - ev;
        // value updating (learning)
        ev += A[i] * PE;
      }
    }
  }
}
