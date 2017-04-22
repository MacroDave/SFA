library(ggthemes); library(stringr);library(reshape2);library(rstan); library(dplyr); library(ggplot2);

# simulate data to see if model recovers parameters-------------------------
set.seed(47)

# Dimensions of the data. 
K <- 5 # firms
T <- 10 # time periods
P <- 2 # predictors
rstar <- .5
time <- rep(1:T, K)
firm <- rep(1:K, each = T)

# structural parameters
sd_time <- 1
alpha <- 10
time_effects <- rnorm(T, alpha, sd_time)
beta <- c(0.7, 0.3)

lambda <-rexp(1, rate=-log(rstar))
mu <- rexp(K*T, rate=lambda)
err <- rnorm(K*T)

# Predictors Matrix
X <- matrix(rnorm(T*K*P), T*K, P)

Y <- rep(NA, T*K)
for (i in 1:(K*T)) {
  Y[i] <- as.numeric(time_effects[time[i]] + X[i,] %*% beta - mu[i] + err[i])
}

ggplot(data = data.frame(time, Y, firm), aes(x = time, y = Y, group = firm)) + geom_line()


# run model ---------------------------------------------------------------
data_list <- list(K = K, 
                  T = T, 
                  P = P, 
                  Y = Y,
                  X = X,
                  rstar=rstar,
                  time = time)


# Compile The Model
compiled_model <- stan_model(file = "simulate_stan_time_effects.stan")
sampled_model <- sampling(compiled_model, data = data_list, iter = 1000, cores = 4)

mu_est <- get_posterior_mean(sampled_model, pars = "mu")[,5]
plot(mu, mu_est)

preds <- sampled_model%>% as.data.frame %>% dplyr::select(contains("Y_predict")) %>% as.matrix
bayesplot::ppc_dens_overlay(Y, preds[1:50,])

# Print estimated parameters from the model
print(sampled_model, pars = c("alpha", "beta", "sigma"))