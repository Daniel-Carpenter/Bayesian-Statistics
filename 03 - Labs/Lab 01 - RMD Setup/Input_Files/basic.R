library(rstan)
library(Intro2R)

basic_data <- list(y=ddt$LENGTH, N=length(ddt$LENGTH))
fit <- stan(file = "basic.stan",
     model_name = "basic",
     data = basic_data,
     chains = 3,
     warmup = 1000,
     cores = 3,
     iter = 5000,
     pars = c("mu")
      )

summary(fit)
library(ggplot2)
library(rstanarm)
color_scheme_set("blue")
mcmc_hist(fit,pars = c("mu"))
mcmc_acf(fit,pars = "mu")

