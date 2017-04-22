//define data
data {
  int K; // number of firms
  int T; // number of time periods
  int P; // number of predictors
  vector[K] Y; // output of each firm
  matrix[K, P] X; // inputs of each firm
  real rstar; //prior median efficiency 
}

// define parameters
parameters {
  real alpha; //constant term 
  vector[P] beta; //coefs on different inputs
  vector<lower = 0>[K] mu; //firm inefficiency 
  real<lower = 0> sigma; //measurement error variance
  real<lower = 0> lambda;
}

// write out the model
model {
  // priors
  alpha ~ normal(1, 1);
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  lambda ~ exponential(-log(rstar));
  
  // predicted firm output given data and parameters
  mu ~ exponential(lambda);
  Y ~ normal(alpha + X*beta - mu, sigma);
}

//Generate Predicted Values to do post. pred. checking
generated quantities {
  vector[K] Y_predict;
  for (k in 1:K) {
    Y_predict[k] = normal_rng(alpha + X[k,1]*beta[1] + X[k,2]*beta[2] - mu[k], sigma);
  }
}
