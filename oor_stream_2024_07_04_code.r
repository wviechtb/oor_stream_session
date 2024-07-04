############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-07-04
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 2.4 - ?
#
# last updated: 2024-07-04

############################################################################

### 2.4: Making the model go

## 2.4.1. Bayes’ theorem.

# as we saw last time, the likelihood function for seeing 6 times water out of
# 9 globe tosses can be computed based on a binomial distribution where the
# data are fixed and we change the true probability (p) to various values
# between 0 and 1

ps <- seq(0, 1, length=1000)
ls <- dbinom(6, size=9, prob=ps)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Likelihood")

# if we assume each value of p is equally plausible to begin with, then the
# prior distribution for p is flat

prior1 <- rep(1, 1000)
prior1 <- prior1 / sum(prior1)
plot(ps, prior1, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Prior Probability")

# then we get the following posterior distribution for p

post1 <- ls * prior1
post1 <- post1 / sum(post1)
plot(ps, post1, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# however, we could also think that values of p between 0 and 0.5 are
# completely impossible, but every value between 0.5 and 1 is equally
# plausible

prior2 <- c(rep(0,500), rep(1,500))
prior2 <- prior2 / sum(prior2)
plot(ps, prior2, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Prior Probability")

# then the posterior will look as follows

post2 <- ls * prior2
post2 <- post2 / sum(post2)
plot(ps, post2, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# finally, we might think that values of p around 0.5 are more plausible than
# values close to 0 or 1; we could express this in terms of the following
# equation

prior3 <- exp(-5 * abs(ps-0.5))
prior3 <- prior3 / sum(prior3)
plot(ps, prior3, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Prior Probability")

# then the posterior will look as follows

post3 <- ls * prior3
post3 <- post3 / sum(post3)
plot(ps, post3, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# let's put all of this into a single figure (Figure 2.6)

par(mfrow=c(3,3))
plot(ps, prior1, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
mtext("prior", side=3, line=2, font=2)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
mtext("likelihood", side=3, line=2, font=2)
plot(ps, post1, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
mtext("posterior", side=3, line=2, font=2)
plot(ps, prior2, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, ls, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, post2, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, prior3, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, ls, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, post3, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
par(mfrow=c(1,1))

## 2.4.3: Grid approximation

# we actually did the grid approximation above (using 1000 grid points), so no
# need to repeat all of this here

## 2.4.4: Quadratic approximation

library(rethinking)

globe.qa <- quap(
   alist(W ~ dbinom(W+L, p), # binomial likelihood
   p ~ dunif(0,1)            # uniform prior
), data=list(W=6,L=3))

# display summary of quadratic approximation
res <- precis(globe.qa)
res

# plot the posterior for a uniform prior from our grid approximation above

plot(ps, post1, type="l", lwd=5, bty="l", xlab="True Probability",
     ylab="Posterior Probability", col="dodgerblue")
post1.qa <- dnorm(ps, mean=res$mean, sd=res$sd)
post1.qa <- post1.qa / sum(post1.qa)
lines(ps, post1.qa, lwd=5)
legend("topleft", inset=.02, legend=c("Grid Approximation", "Quadratic Approximation"),
       lwd=5, col=c("dodgerblue","black"))

# if we use a beta distribution to define our prior plausibilities for the
# different values of p, then for a binomial likelihood function, it turns out
# that there is a nice analytic derivation of the posterior distribution; see:
# https://en.wikipedia.org/wiki/Beta_distribution#Effect_of_different_prior_probability_choices_on_the_posterior_beta_distribution

# so let's say we use Beta(alpha, beta) to describe the prior distribution;
# then it turns out that in this case, the posterior distribution is also a
# beta distribution with parameters B(alpha + s, beta + f), where 's' is the
# number of successes and 'f' is the number of failures (in the present
# example, these are the number of times we see water and land, respectively)

# note" a uniform prior is actually a Beta(1, 1) distribution, so the exact
# posterior distribution in our example is Beta(1+6, 1+3)

# let's check that we get essentially the same posterior as what we found
# using the grid approximation; we plot the exact posterior on top in red

post1.exact <- dbeta(ps, 1+6, 1+3)
post1.exact <- post1.exact / sum(post1.exact)
lines(ps, post1.exact, lwd=5, col="firebrick")

# we see no difference between the blue and red lines, so the grid
# approximation is essentially perfect here

# say now that we have observed 4*6 successes and 4*3 failures (so we get the
# same peak for the likelihood, but we have four times as much data)

globe.qa <- quap(
   alist(W ~ dbinom(W+L, p), # binomial likelihood
   p ~ dunif(0,1)            # uniform prior
), data=list(W=4*6,L=4*3))

# display summary of quadratic approximation

res <- precis(globe.qa)
res

post1.exact <- dbeta(ps, 1+4*6, 1+4*3)
plot(ps, post1.exact, type="l", lwd=5, bty="l", xlab="True Probability",
     ylab="Posterior Probability", col="dodgerblue")
post1.qa <- dnorm(ps, mean=res$mean, sd=res$sd)
lines(ps, post1.qa, lwd=5)
legend("topleft", inset=.02, legend=c("Exact Posterior", "Quadratic Approximation"),
       lwd=5, col=c("dodgerblue","black"))

# let's go back to the smaller dataset

globe.qa <- quap(
   alist(W ~ dbinom(W+L, p), # binomial likelihood
   p ~ dunif(0,1)            # uniform prior
), data=list(W=6,L=3))

# display summary of quadratic approximation

res1 <- precis(globe.qa)
res1

# function that computes the posterior for a given value of p, using a uniform
# prior distribution

postfun <- function(p, W, L) {

   likelihood <- dbinom(W, size=W+L, prob=p)
   prior <- dunif(p, 0, 1)
   posterior <- likelihood * prior
   logposterior <- log(posterior)
   cat("p = ", round(p, 3), "logposterior" = round(logposterior, 3), "\n")
   logposterior

}

# now optimize postfun() over p (i.e., find the peak of the posterior)

res2 <- optim(par=0.5, postfun, method="L-BFGS-B", lower=0.001, upper=0.999,
              control=list(fnscale=-1), W=6, L=3, hessian=TRUE)
res2

# we see that the peak is at p = 0.666666 = 6/9

# the Hessian matrix gives the second derivative around this peak; as noted in
# 'Overthinking' box, -1 * the inverse of this Hessian matrix is an estimate
# of the variance and so the square-root of this is the standard deviation

sqrt(-1/res2$hessian[1,1])

# this is exactly identical to what we get for the SD from quap()

res1$sd

## 2.4.5: Markov chain Monte Carlo

n_samples <- 100000
p <- rep(NA , n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for (i in 2:n_samples) {
   p_new <- rnorm(1, p[i-1], 0.1)
   if (p_new < 0) p_new <- abs(p_new)
   if (p_new > 1) p_new <- 2 - p_new
   q0 <- dbinom(W, W+L, p[i-1])
   q1 <- dbinom(W, W+L, p_new)
   p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

# compare the exact posterior distribution with a kernel density estimate
# based on the samples of the posterior we obtained from MCMC

post1.exact <- dbeta(ps, 1+6, 1+3)
plot(ps, post1.exact, type="l", lwd=5, bty="l", xlab="True Probability",
     ylab="Posterior Probability", col="dodgerblue")
lines(density(p, from=0, to=1), lwd=5, main="", bty="l")
legend("topleft", inset=.02, legend=c("Exact Posterior", "MCMC"),
       lwd=5, col=c("dodgerblue","black"))
