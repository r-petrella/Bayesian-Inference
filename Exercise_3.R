#############################################
########  EXERCISE 3: Logistic model  ########
#############################################
library(rstanarm)
library(loo)
library(bayesplot)
library(rstan)

data3<-read.csv("Data_Ex_3.csv")

#overview
str(data3)

#state must be a factor
data3$state<-factor(data3$state)


########################
## Several models fitted
########################

# Simple Logistic model 
mod_ex3a<- stan_glm(bush~black+female, data=data3,
                    family = "binomial")
summary(mod_ex3a)
stan_trace(mod_ex3a, nrow = 3, ncol=1)
stan_ac(mod_ex3a)

# Logistic model with random intercept
mod_ex3b<- stan_glmer(bush~black+female+(1|state), data=data3,
                    family = "binomial", iter=4000, warmup = 2000)
summary(mod_ex3b)
stan_trace(mod_ex3b, nrow = 4, ncol = 1,
           pars = c("(Intercept)", "blackyes", "femaleyes", "Sigma[state:(Intercept),(Intercept)]"))
stan_ac(mod_ex3b, pars = c("(Intercept)", "blackyes", "femaleyes", "Sigma[state:(Intercept),(Intercept)]"))


# Comparison
waic(mod_ex3a)
waic(mod_ex3b)

main_pars <- c("(Intercept)","blackyes","femaleyes","Sigma[state:(Intercept),(Intercept)]")
summary(mod_ex3b, pars = main_pars, digits = 3)

#Posterior predictive checks

y_tilde <- posterior_predict(mod_ex3b)

ppc_stat(y = data3$bush, yrep = y_tilde, stat = "mean")
ppc_stat(y = data3$bush, yrep = y_tilde, stat = "sd")
ppc_stat(y = data3$bush, yrep = y_tilde, stat = "sum")


# Posterior inference
# Estimated probability for subject 4

theta <- posterior_linpred(mod_ex3b, transform = TRUE)

hist(theta[,4])
mean(theta[,4]);sd(theta[,4])
quantile(theta[,4], 
         probs = c(0.025,0.5,0.975))

