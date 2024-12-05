############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-12-05
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.4 - ?
#
# last updated: 2024-12-05

############################################################################

### 4.4: Linear prediction

# load the rethinking package
library(rethinking)

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# select only those who are 18 years or older
dat <- dat[dat$age >= 18,]

# plot the height of the individuals versus their weight
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")

# correlation between height and weight
cor(dat$height, dat$weight)

## 4.4.1: The linear model strategy

# 4.4.1.3: Priors

# simulate 100 alpha and beta values based on the prior distributions
set.seed(2971)
n <- 100
sim <- data.frame(a = rnorm(n, mean=178, sd=20),
                  b = rnorm(n, mean=0, sd=10))

# Figure 4.5 (left): plot the regression lines implied by these values
plot(NA, xlim=range(dat$weight), ylim=c(-100,400), xlab="weight", ylab="height", bty="l")
abline(h=0, lty=2)
abline(h=272, lty=1)
mtext("b ~ dnorm(0,10)")
xbar <- mean(dat$weight)
xs <- seq(min(dat$weight), max(dat$weight))
invisible(apply(sim, 1, function(par) lines(xs, par["a"] + par["b"] * (xs - xbar), col=rgb(0,0,0,0.2), lwd=2)))

# simulate 1,000,000 values from a log-normal distribution with a mean of 0 and an SD of 1
b <- rlnorm(1e6, mean=0, sd=1)
plot(density(b, from=-0.1, to=5), lwd=2, bty="l", main="")

# simulate 100 alpha and beta values based on the prior distributions
set.seed(2971)
sim <- data.frame(a = rnorm(n, mean=178, sd=20),
                  b = rlnorm(n, mean=0, sd=1))

# Figure 4.5 (right): plot the regression lines implied by these values
plot(NA, xlim=range(dat$weight), ylim=c(-100,400), xlab="weight", ylab="height", bty="l")
abline(h=0, lty=2)
abline(h=272, lty=1)
mtext("log(b) ~ dnorm(0,1)")
xbar <- mean(dat$weight)
xs <- seq(min(dat$weight), max(dat$weight))
invisible(apply(sim, 1, function(par) lines(xs, par["a"] + par["b"] * (xs - xbar), col=rgb(0,0,0,0.2), lwd=2)))

## 4.4.2: Finding the posterior distribution

model <- alist(height ~ dnorm(mu, sigma),
               mu <- a + b*(weight - mean(weight)),
               a ~ dnorm(178, 20),
               b ~ dlnorm(0, 1),
               sigma ~ dunif(0, 50))
res1 <- quap(model, data=dat)
res1
precis(res1, prob=0.95)

############################################################################

post <- extract.samples(res1, n=1e5)
head(post)

post$mu <- apply(post, 1, function(par) par["a"] + par["b"] * (sample(dat$weight, 1) - xbar))
hist(post$mu, breaks=50, freq=FALSE)

curve(dnorm(x, mean=mean(post$mu), sd=sd(post$mu)), lwd=5, add=TRUE)

X <- cbind(1, dat$weight - mean(dat$weight))
means <- c(X %*% coef(res1)[1:2])
vars <- X %*% vcov(res1)[1:2,1:2] %*% t(X)

heights <- seq(min(dat$height), max(dat$height), length=1000)
dens <- matrix(NA, nrow=nrow(dat), ncol=1000)

for (i in 1:nrow(dat)) {
   dens[i,] <- dnorm(heights, mean=means[i], sd=sqrt(diag(vars)[i]))
}

dens <- apply(dens, 2, sum)
trapezoid <- function(x,y) sum(diff(x)*(y[-1]+y[-length(y)]))/2
dens <- dens / trapezoid(heights, dens)

lines(heights, dens, lwd=5, col="red")

############################################################################

# same model as above, but parameterized in such a way that we get the
# posterior distribution of log(b)
res2 <- quap(alist(height ~ dnorm(mu, sigma),
                   mu <- a + exp(log_b)*(weight - mean(weight)),
                   a ~ dnorm(178, 20),
                   log_b ~ dnorm(0, 1),
                   sigma ~ dunif(0, 50)), data=dat)
res2
precis(res2, prob=0.95)

# https://en.wikipedia.org/wiki/Delta_method
# delta method:
# derivative of exp(x) with respect to x is = exp(x)
# then exp(log_b) ~ N(exp(mean-log_b), sd = sqrt(variance(log_b) * exp(mean-log_b)^2))

coef(res1)["b"]
exp(coef(res2)["log_b"])

se(res1)["b"]
se(res2)["log_b"] * exp(coef(res2)["log_b"])

############################################################################

## 4.4.3: Interpreting the posterior distribution

# 4.4.3.1: Tables of marginal distributions

# table with the estimates, their SDs, and 95% compatibility (credible) intervals
precis(res1, prob=0.95)

# variance-covariance matrix of the estimates
round(vcov(res1), digits=3)

# corresponding correlation matrix
round(cov2cor(vcov(res1)), digits=3)

# plot of the marginal posterior distributions based on sampled values and
# scatterplots of these sampled values against each other
pairs(res1)

# 4.4.3.2: Plotting posterior inference against the data

# Figure 4.6: plot the height of the individuals versus their weight
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")

# extract 10^4 samples from the posterior distribution
post <- extract.samples(res1)
head(post)

# compute the mean of the intercept and slope samples
a_map <- mean(post$a)
b_map <- mean(post$b)

# add the regression line based on these means to the plot
curve(a_map + b_map*(x-xbar), lwd=5, add=TRUE)

# 4.4.3.3: Adding uncertainty around the mean

# select the first 10 people from the dataset
sub <- dat[1:10,]

# refit the model based on this subset
res3 <- quap(model, data=sub)

# sample 20 values from the posterior
post <- extract.samples(res3, n=20)

# Figure 4.7 (upper left): plot the height of the individuals versus their
# weight for the subset with the 20 regression lines added based on the 20
# sampled values of the intercept and slope
plot(height ~ weight, data=sub, pch=21, bg="gray", bty="l")
invisible(apply(post, 1, function(par) curve(par["a"] + par["b"] * (x-mean(sub$weight)), col=rgb(0,0,0,0.2), lwd=2, add=TRUE)))

# Figure 4.7 (lower right): same plot but based on the full sample
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")
post <- extract.samples(res1, n=20)
invisible(apply(post, 1, function(par) curve(par["a"] + par["b"] * (x-mean(dat$weight)), col=rgb(0,0,0,0.2), lwd=2, add=TRUE)))

# 4.4.3.4: Plotting regression intervals and contours

# generate 10^4 sampled values of mu for individuals with weight = 50
post <- extract.samples(res1)
mu_at_50 <- post$a + post$b * (50 - xbar)

# Figure 4.8: kernel density estimate of this distribution
plot(density(mu_at_50), col="#1e59ae", lwd=5, xlab="mu|weight=50", main="", bty="l")

# 95% compatibility interval
quantile(mu_at_50, prob=c(.025, .975))

############################################################################

post$height <- apply(post, 1, function(par) rnorm(1, par["a"] + par["b"] * (sample(dat$weight, 1) - xbar), sd=par["sigma"]))
hist(post$height, breaks=50, freq=FALSE)
hist(dat$height, breaks=30, freq=FALSE)
