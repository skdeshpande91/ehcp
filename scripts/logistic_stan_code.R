logit_code1 <- '
data{
  int N_train; // number of training observations
  int N_test; // number of testing observations
  int p; // number of covariates
  row_vector[p] x_train[N_train]; // covariates for training data
  row_vector[p] x_test[N_test];// covariates for testing data
  int<lower = 0, upper = 1> y[N_train]; // responses
}
parameters{
  vector[p] beta; // covariate effects
  real beta_0; // intercept
}
transformed parameters{
  vector[N_train] log_odds;
  for(n in 1:N_train){
    log_odds[n] = x_train[n] * beta + beta_0;
  }
}
model{
  y ~ bernoulli_logit(log_odds);
  beta ~ normal(0,1);
  beta_0 ~ normal(0,1);
}
generated quantities{
  real<lower = 0, upper = 1> phat_train[N_train];
  real<lower = 0, upper = 1> phat_test[N_test];

  for(n in 1:N_train){
    phat_train[n] = 1/(1 + exp(-1*log_odds[n]));
  }
  for(n in 1:N_test){
    phat_test[n] = 1/(1 + exp(-1*(beta_0 + x_test[n] * beta)));
  }
}
'
