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

res1 <- quap(alist(height ~ dnorm(mu, sigma),
                   mu <- a + b*(weight - xbar),
                   a ~ dnorm(178, 20),
                   b ~ dlnorm(0, 1),
                   sigma ~ dunif(0, 50)), data=dat)
res1
precis(res1, prob=0.95)

############################################################################

post <- extract.samples(res1, n=1e5)
head(post)

post$mu <- apply(post, 1, function(par) par["a"] + par["b"] * (sample(dat$weight, 1) - xbar))
hist(post$mu, breaks=50, freq=FALSE)

curve(dnorm(x, mean=mean(post$mu), sd=sd(post$mu)), lwd=5, add=TRUE)

X <- cbind(1, dat$weight - xbar)
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

