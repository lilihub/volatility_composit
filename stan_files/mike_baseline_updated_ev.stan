// stan model for aversive learning task
data {
  int<lower=1>           N;
  int<lower=1>           T;
  int<lower=1, upper=T>  Tsubj[N]; // for subjects with missing trials
  int<lower=0, upper=1>  choiceB[N, T];
  real                   outcome_blue[N, T];  // no lower and upper bounds. This is whether or not blue STOLE
  real                   amount_blue[N, T]; // this refers to the number of points lost by this stimulus, if it steals
  real                   amount_orange[N, T]; // this refers to the number of points lost by this stimulus, if it steals
  real                   initV_ev[N];
}

// transformed data {
//   real initV_ev;  // initial values for EV blue
//   initV_ev = 0.5;
// }

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
    A[i]   = inv_logit(mu_pr[1] + sigma[1]  * A_pr[i]);
    beta[i] = inv_logit(mu_pr[2] + sigma[2] * beta_pr[i]) * 10;
    gamma[i] = inv_logit(mu_pr[3] + sigma[3] * gamma_pr[i]) * 10;
  }
}

model {
  real ev[N];
  // Hyperparameters
  mu_pr  ~ normal(0, 5);
  sigma ~ normal(0, 5);
  // individual parameters
  // always want this 0,1 so that you can use in matt trick (as raw parameter)
  A_pr ~ normal(0, 1.0);
  beta_pr ~ normal(0, 1.0);
  gamma_pr ~ normal(0, 1.0);
  ev = initV_ev;
  // subject loop and trial loop
  for (i in 1:N) {
    vector[2] gvalue;
    vector[2] fvalue;
    real PE;      // prediction error
    for (t in 1:Tsubj[i]) {
      fvalue[1] = fmax(fmin((gamma[i]*(ev[i] - 0.5) + 0.5), 1), 0); // fvalue of the blue choice (magnitudes of point loss)
      fvalue[2] = fmax(fmin((gamma[i]*(1 - ev[i] - 0.5) + 0.5), 1), 0); // fvalue of the orange choice  (magnitudes of point loss)
      //fvalue[2] = 1 - fvalue[1];
      gvalue[1] = fvalue[1] * amount_blue[i, t]; // gvalue of the blue choice (estimated negative values, which are a combination of point loss mangnitude and probability of loss)
      gvalue[2] = fvalue[2] * amount_orange[i, t]; // gvalue of the orange choice (estimated negative values, which are a combination of point loss mangnitude and probability of loss)

      // compute action probabilities
      choiceB[i, t] ~ bernoulli_logit((-beta[i]) * (gvalue[1]-gvalue[2]));
      // prediction error of the blue choice
      PE = outcome_blue[i, t] - ev[i];
      // value updating (learning) only updating the ev of the blue choice
      ev[i] += A[i] * PE;
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
  real ev[N];
  ev = initV_ev;

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }
  mu_A = inv_logit(mu_pr[1]);
  mu_beta = inv_logit(mu_pr[2]) * 10;
  mu_gamma = inv_logit(mu_pr[3]) * 10;

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[2] gvalue;
      vector[2] fvalue;
      real PE;      // prediction error

      log_lik[i] = 0;

      for (t in 1:Tsubj[i]) {

        fvalue[1] = fmax(fmin((gamma[i]*(ev[i] - 0.5) + 0.5), 1), 0);
        fvalue[2] = fmax(fmin((gamma[i]*(1 - ev[i] - 0.5) + 0.5), 1), 0);
        gvalue[1] = fvalue[1] * amount_blue[i, t];
        gvalue[2] = fvalue[2] * amount_orange[i, t];

        // compute log likelihood of current trial
        log_lik[i] += bernoulli_logit_lpmf(choiceB[i, t] | (-beta[i]) * (gvalue[1] - gvalue[2]));
        // generate posterior prediction for current trial
        y_pred[i, t] = bernoulli_logit_rng(-beta[i] * (gvalue[1] - gvalue[2]));
        // y_pred[i, t] = categorical_logit_rng((-beta[i]) * gvalue);
        // prediction error
        PE = outcome_blue[i, t] - ev[i];
        // value updating (learning)
        ev[i] += A[i] * PE;
      }
    }
  }
}
