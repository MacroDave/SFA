//define data
data {
  int K; // number of firms
  int T; // number of time periods
  int P; // number of predictors
  vector[K*T] Y; // output of each firm
  matrix[K*T, P] X; // inputs of each firm
  real rstar; //prior median efficiency 
  int time[K*T];
}

// define parameters
parameters {
  real alpha; //constant term 
  real<lower = 0> time_effect_sd;
  vector[P] beta; //coefs on different inputs
  vector<lower = 0>[K*T] mu; //firm inefficiency 
  real<lower = 0> sigma; //measurement error variance
  real<lower = 0> lambda;
  vector[T*K] z;
}
transformed parameters {
  vector[T*K] time_effect;
  time_effect = alpha + time_effect_sd * z;
}

// write out the model
model {
  // priors
  alpha ~ normal(1, 1);
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  lambda ~ exponential(-log(rstar));
  time_effect_sd ~ cauchy(0, 1);
  z ~ normal(0, 1);
  
  // predicted firm output given data and parameters
  mu ~ exponential(lambda);
  for(i in 1:(K*T)){
    Y[i] ~ normal(time_effect[time[i]] + X[i]*beta - mu, sigma);
  }
}

//Generate Predicted Values to do post. pred. checking
generated quantities {
  vector[K*T] Y_predict;
  for (k in 1:(T*K)) {
    Y_predict[k] = normal_rng(time_effect[time[k]] + X[k]*beta - mu[k], sigma);
  }
}
