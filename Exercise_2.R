#########################################
########  EXAMPLE 2: radon data  ########
#########################################
library(rstanarm)
library(loo)
library(bayesplot)
library(rstan)

data2<-read.csv("Data_Ex_2.csv")

#overview
str(data2)
pairs(data2[3:4])

########################
## Several models fitted
########################

# Random intercept model
mod_ex2a<-stan_lmer(log_radon~(1|county),data = data2)
prior_summary(mod_ex2a)
summary(mod_ex2a)
stan_trace(mod_ex2a, pars = c("(Intercept)", "sigma", "Sigma[county:(Intercept),(Intercept)]"),
           nrow=3, ncol=1)
stan_ac(mod_ex2a, 
        pars = c("(Intercept)", "sigma", "Sigma[county:(Intercept),(Intercept)]"))

# Random intercept model with county level covariate and measure level covariate
mod_ex2b<-stan_lmer(log_radon~log_uranium+floor+(1|county),data = data2)
summary(mod_ex2b)

stan_trace(mod_ex2b, 
           pars = c("(Intercept)", "log_uranium", "floorfirst","sigma",
                    "Sigma[county:(Intercept),(Intercept)]"),
           nrow= 5, ncol = 1)
stan_ac(mod_ex2b, 
           pars = c("(Intercept)", "log_uranium", "floorfirst","sigma",
                    "Sigma[county:(Intercept),(Intercept)]"))

# Random intercept model with county level covariate and random coefficient
mod_ex2c<-stan_lmer(log_radon~log_uranium+floor+(1+floor|county),data = data2)
summary(mod_ex2c)

mod_ex2c<-update(mod_ex2c, iter=5000)
summary(mod_ex2c)

stan_trace(mod_ex2c, pars = c("(Intercept)", "log_uranium","floorfirst", "sigma", "Sigma[county:(Intercept),(Intercept)]", "Sigma[county:floorfirst,floorfirst]"))
stan_ac(mod_ex2c, pars = c("(Intercept)", "log_uranium","floorfirst", "sigma", "Sigma[county:(Intercept),(Intercept)]", "Sigma[county:floorfirst,floorfirst]"))


#####################
## MODEL CHOICE: WAIC
#####################

waic(mod_ex2a)
waic(mod_ex2b)#better
waic(mod_ex2c)

main_pars<-c("(Intercept)", "log_uranium", "floorfirst","sigma", "Sigma[county:(Intercept),(Intercept)]")

#summary of the better model
summary(mod_ex2b, pars = main_pars, digits = 3)

## Deeper check of the convergence
stan_ac(mod_ex2b, pars = main_pars)

# posterior intervals
posterior_interval(mod_ex2b, prob = 0.8, pars = "log_uranium")
posterior_interval(mod_ex2b, prob = 0.9, pars = "log_uranium")

## Posterior predictive 
y_tilde<-posterior_predict(mod_ex2b)
ppc_dens_overlay(y = data2$log_radon, yrep = y_tilde[1100:1200,])
ppc_stat(y = data2$log_radon, yrep = y_tilde, stat = "mean")
ppc_stat(y = data2$log_radon, yrep = y_tilde, stat = "sd")
ppc_stat(y = data2$log_radon, yrep = y_tilde, stat = "max")
ppc_stat(y = data2$log_radon, yrep = y_tilde, stat = "min")


