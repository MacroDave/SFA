library(ggthemes); library(stringr);library(reshape2);library(rstan); library(dplyr); library(ggplot2);

# simulate data to see if model recovers parameters-------------------------
set.seed(47)

# Dimensions of the data. 
K <- 100 # firms
T <- 100 # time periods
P <- 2 # predictors
rstar <- .5

# structural parameters
alpha <- 1
beta <- c(0.7, 0.3)
lambda <-rexp(1, rate=-log(rstar))
mu <- rexp(K, rate=lambda)
err <- rnorm(K)

# Product characteristics matrix
X <- matrix(rnorm(K*P), K, P)

# The generative model for price
Y <- alpha + X %*% beta - mu + err
Y <- Y[,1]

# run model ---------------------------------------------------------------
data_list <- list(K = K, 
                  T = T, 
                  P = P, 
                  Y = Y,
                  X = X,
                  rstar=rstar)

# Compile The Model
compiled_model <- stan_model(file = "simulate_stan.stan")

sampled_model <- sampling(compiled_model, data = data_list, iter = 1000, cores = 4)

mu_est <- get_posterior_mean(sampled_model, pars = "mu")[,5]
plot(mu, mu_est)


library(bayesplot)

preds <- sampled_model%>% as.data.frame %>% dplyr::select(contains("Y_predict")) %>% as.matrix

bayesplot::ppc_dens_overlay(Y, preds[1:50,])



#Pull predictions from estimated model
predictions <- as.data.frame(test_optim) %>% 
  dplyr::select(contains("Y_predict")) %>% 
  melt()

predictions %>% 
  ggplot() +
  geom_line(aes(x = value, group = variable), colour = "orange", stat = "density", alpha = 0.1, adjust = .8) +
  geom_density(data = data_frame(Y), aes(x = Y), colour = "black", adjust = 0.8) +
  ggthemes::theme_economist() +
  ggtitle("Actual outcomes and posterior predictive replications") +
  annotate("text", x = 0.99, y = 0.08, label = "Density of actual outcomes", hjust = 0) +
  annotate("text", x = 0.99, y = 0.09, label = "Posterior replications", colour = "orange", hjust = 0) 

