##############################################
## Example: conjugate Beta - Binomial model ##
##############################################

# prior parameters 
a <- 1
b <- 1

# sample evidences
r <- 15 # number of successes
n <- 20 # number of trials

# posterior parameters
a_prime <- a + r
b_prime <- n - r + b

# size of the random sample
S <- 1e6

# random sample from the posterior
post_sample <- rbeta(n = S, 
                     shape1 = a_prime, 
                     shape2 = b_prime)

# visualization of the posterior (samples)
hist(post_sample, breaks = 30, probability = T, 
     main = "Posterior of theta", xlab = "theta")

# theoretical distribution of the posterior
curve(expr = dbeta(x, shape1 = a_prime, shape2 = b_prime), 
      from = 0, to = 1,col = 2, add = T)

# summary statistics computation
mean(post_sample); a_prime / (a_prime + b_prime)
sd(post_sample); 
sqrt((a_prime * b_prime) / 
       ((a_prime + b_prime) ^ 2 * (a_prime + b_prime + 1)))

# quantiles
p <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
quantile(post_sample, probs = p) # sample
qbeta(p = p, shape1 = a_prime, shape2 = b_prime) # theoretical

# Monte Carlo SE
sd(post_sample)/sqrt(S)



