library(rstanarm)
library(rstan)
library(ggplot2)
data(roaches)
# Rescale
roaches$roach1 <- roaches$roach1 / 100

# 1. Assume a simple Poisson regression model without random effects 
# considering roach1 and senior as independent variables 
# (model a). Write the theoretical form of the
# model assuming weakly-informative prior distributions for 
# the coeffcients and program the model in rstanarm.

mod_a <- stan_glm( formula = y ~ roach1 + senior ,
                        data = roaches ,
                        family = "poisson")
prior_summary(mod_a)
summary(mod_a)


# 2. Monitor the convergence of the algorithm (referred to model a).
stan_trace(mod_a , nrow =3 , ncol =1)  # very rapid convergence
# stable   # no excessive random variability

stan_ac(mod_a)  # related to trace
# fast convergence and close to proper mc

# 3. Assume now that we are interested in update model a 
# in order to estimate also group-specific effects with 
# respect to the type of treatment (model b). Write the
# theoretical form of the model assuming a N (0,10) prior
# distribution with automatic scale adjustments for the 
# coeffcients and program the model in rstanarm.

model_b <- stan_glmer(formula = y~roach1+senior+(1|treatment),
                    data= roaches,
                    family="poisson",
                    prior = normal(0,10, autoscale = T),
                    prior_intercept = normal(0,10, autoscale = T))


summary(model_b)

# n_eff to far from 4000 when intercept is used
# warning on convergence
# need of an update

model_b <- update(model_b, iter = 4000, adapt_delta=0.99)

summary(model_b)

# 4. Monitor the convergence of the algorithm 
# (referred to model b) and in particular
#discuss the interpretation of n_eff.
stan_trace(model_b)

# random variability seems to be too high

stan_ac(model_b)
# too much correlation, slow convergence

# 5. Compare the two models in terms of goodness of ot.
# Which one provides a better fit of the original data?
  
waic(mod_a)
waic(model_b) # better because lower waic

# 6. Suppose that we are interested in verifying that the chosen
# model is able to reproduce
# the mean number of post-treatment roaches. Check this assumption.
# How do the posterior predictive checks work?
# Provide a brief explanation.

y_tildeb <- posterior_predict(model_b)
ppc_dens_overlay(y= roaches$y, yrep = y_tildeb[100:200,])
ppc_stat(y=roaches$y, yrep = y_tildeb, stat = "mean")

# 7. Report the 90% credible interval of the predicted parameters.
# returns the posterior distribution of the conditional
# expectation

mu <- posterior_epred(model_b)

quantile(mu, probs = c(0.05,0.5,0.95))

# 8. Provide an overall comment of the performed analysis.
# How could you improve, if necessary, the fitted models in order
# to better fit the initial dataset?

# according to waic we should choose model b. However the model 
# has a slow convergence and there is too much autocorrelation

# let us try to remove the intercept

model_b1 <- stan_glm(formula = y~roach1+senior+treatment,
                      data= roaches,
                      family="poisson",
                      prior = normal(0,10, autoscale = T))
                      

stan_trace(model_b1)
stan_ac(model_b1)
waic(model_b1)
waic(model_b)

# higher waic but lower random variability and autocorrelation