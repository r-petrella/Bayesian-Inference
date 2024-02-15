################################################
########  EXAMPLE 1: linear regression  ########
################################################

data1<-read.csv("Data_Ex_1.csv")

#overview
str(data1)
pairs(data1)

# frequentist equivalent model
summary(lm(time~amount+distance, data=data1))

# load package
library(rstanarm)
library(loo)
library(bayesplot)
library(rstan)

# model estimation

#understanding the rstanarm default priors
mod_ex1<-stan_glm(formula = time~amount+distance,
                  data = data1, 
                  family = "gaussian")
prior_summary(mod_ex1)

#understanding the flat prior
mod_ex1<-stan_glm(formula = time~amount+distance,
                  data = data1, 
                  family = "gaussian",
                  prior = NULL)
prior_summary(mod_ex1)

#changing the prior
################
## MODEL FITTING
################
mod_ex1<-stan_glm(formula = time~amount+distance, 
                  data = data1, family = "gaussian",
                  prior=normal(0,100),
                  prior_intercept = normal(0,100),
                  prior_aux = cauchy(0,1))

prior_summary(mod_ex1)

####################
## CONVERGENCE CHECK
####################

## Graphical
#traceplot
stan_trace(mod_ex1, nrow = 3, ncol = 1, inc_warmup = T)
plot(mod_ex1, plotfun = "trace")

#autocorrelation function
library(ggplot2)
stan_ac(mod_ex1)
plot(mod_ex1, plotfun = "ac")

## Numerical: Rhat and n_eff
summary(mod_ex1)
plot(mod_ex1, plotfun = "rhat")
mean(data1$time)


##############################
## POSTERIOR PREDICTIVE CHECKS
##############################

# Generate from the posterior predictive
y_tilde<-posterior_predict(mod_ex1)

# Densities comparison
ppc_dens_overlay(y = data1$time, yrep = y_tilde[1000:1080,])

#Posterior predictive checks
ppc_stat(y = data1$time, yrep = y_tilde, stat = "mean")
ppc_stat(y = data1$time, yrep = y_tilde, stat = "sd")
ppc_stat(y = data1$time, yrep = y_tilde, stat = "max")
ppc_stat(y = data1$time, yrep = y_tilde, stat = "min")


######################
## POSTERIOR INFERENCE
######################

summary(mod_ex1,digits = 4)

plot(mod_ex1)
stan_hist(mod_ex1)

# linear predictor
mu <- posterior_linpred(mod_ex1)
#posterior distribution of the observation 10
hist(mu[,10],breaks=30)
mean(mu[,10]);sd(mu[,10])
quantile(mu[,10], probs = c(0.05,0.5,0.95))

# posterior inference on an useful quantity
# The bayesian R^2

#extract the posterior sample of interest
sigma_post<-as.matrix(mod_ex1,pars = "sigma")
#build the posterior distribution
n<-nrow(data1)
var_y<-var(data1$time)*(n-1)/n
R2bayes<-1-sigma_post^2/var_y
#posterior inference
mean(R2bayes);sd(R2bayes)
quantile(R2bayes, probs = c(0.025,0.5,0.975))
hist(R2bayes, breaks=30)





