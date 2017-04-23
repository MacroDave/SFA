library(ggthemes); library(stringr);library(reshape2);library(rstan); library(dplyr); library(ggplot2);

# simulate data to see if model recovers parameters-------------------------

# Dimensions of the data. 
K <- 50 # firms
T <- 30 # time periods
P <- 2 # predictors
rstar <- .5
time <- rep(1:T, K)
firm <- rep(1:K, each = T)

# structural parameters
sd_time <- 0.5
alpha <- 1
time_effects <- rnorm(T, alpha, sd_time)
beta <- c(0.7, 0.3)

lambda <-rexp(1, rate=-log(rstar))
mu <- rexp(K*T, rate=lambda)
err <- rnorm(K*T)

# Predictors Matrix
X <- matrix(rnorm(T*K*P), T*K, P)


Y <- time_effects[time] + X %*% beta - mu + err


ggplot(data = data.frame(time, Y, firm), aes(x = time, y = Y, group = firm)) + geom_line()


data_list <- list(K = K, 
                  T = T, 
                  P = P, 
                  Y = as.numeric(Y),
                  X = X,
                  rstar=rstar,
                  time = time)


# Compile The Model
compiled_model <- stan_model(file = "simulate_stan_time_effects.stan")
sampled_model <- sampling(compiled_model, data = data_list, iter = 1000, cores = 4)

mu_est <- get_posterior_mean(sampled_model, pars = "mu")[,5]
plot(mu, mu_est)
abline(0, 1)

ts_est <- get_posterior_mean(sampled_model, pars = "time_effect")[,5]
plot(time_effects, ts_est)
abline(0, 1)

preds <- sampled_model%>% as.data.frame %>% dplyr::select(contains("Y_predict")) %>% as.matrix
bayesplot::ppc_dens_overlay(as.vector(Y), preds[1:50,])



# Print estimated parameters from the model
print(sampled_model, pars = c("alpha", "beta", "sigma", "lambda"))

alpha
beta
lambda
