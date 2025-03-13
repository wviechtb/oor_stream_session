############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-13
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.2 - ?
#
# last updated: 2025-03-13

############################################################################

### 5.2: Masked relationship

# load the rethinking package
library(rethinking)

# get the milk dataset and put it into 'dat'
dat <- get(data(milk))

# inspect the first 6 rows
head(dat)

# standardize the three variables of interest
dat$K <- c(scale(dat$kcal.per.g))
dat$N <- c(scale(dat$neocortex.perc))
dat$M <- c(scale(log(dat$mass)))

# plot the data
plot(K ~ N, data=dat, pch=21, bg="gray", bty="l",
     xlab="Neocortext Percent (std)", ylab="Kilocal per g (std)")

# model predicting K from N
model1 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bN*N,
                a ~ dnorm(0, 1),
                bN ~ dnorm(0, 1),
                sigma ~ dexp(1))

# fit the model
res1 <- quap(model1, data=dat)

# get an error message, due to the missing values in neocortex.perc (and hence N)
dat$neocortex.perc

# keep rows where K, N, and M are complete (i.e., not missing)
dat <- dat[complete.cases(dat$K,dat$N,dat$M),]
dat

# fit the model using the complete data
res1 <- quap(model1, data=dat)

# sample 1000 values from the prior distributions
prior <- data.frame(extract.prior(res1))
head(prior)

# Figure 5.8 (left): plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortext Percent (std)",
     ylab="Kilocal per g (std)", bty="l")
invisible(apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.5, col="gray30")))

# model predicting K from N (using tighter priors for the intercept and slope)
model1 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bN*N,
                a ~ dnorm(0, 0.2),
                bN ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model
res1 <- quap(model1, data=dat)

# sample 1000 values from the prior distributions
prior <- data.frame(extract.prior(res1))
head(prior)

# Figure 5.8 (right): plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortext Percent (std)",
     ylab="Kilocal per g (std)", bty="l")
invisible(apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.5, col="gray30")))

# examine summary statistics for the posterior distributions
precis(res1, prob=0.95)

# Figure 5.9 (top left): plot the data and add the regression line (plus 95% CI)
plot(K ~ N, data=dat, pch=21, bg="gray", bty="l",
     xlab="Neocortext Percent (std)", ylab="Kilocal per g (std)")
xseq <- seq(from=min(dat$N)-0.15, to=max(dat$N)+0.15, length.out=30)
mu <- link(res1, data=list(N=xseq))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)
lines(xseq, mu_mean, lwd=2)
shade(mu_pi, xseq)

# model predicting K from M
model2 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bM*M,
                a ~ dnorm(0, 0.2),
                bM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model
res2 <- quap(model2, data=dat)
precis(res2, prob=0.95)

# Figure 5.9 (top right): plot the data and add the regression line (plus 95% CI)
plot(K ~ M, data=dat, pch=21, bg="gray", bty="l",
     xlab="Log Body Mass (std)", ylab="Kilocal per g (std)")
xseq <- seq(from=min(dat$M)-0.15, to=max(dat$M)+0.15, length.out=30)
mu <- link(res2, data=list(M=xseq))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)
lines(xseq, mu_mean, lwd=2)
shade(mu_pi, xseq)

# model predicting K from N and M
model3 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bN*N + bM*M,
                a ~ dnorm(0, 0.2),
                bN ~ dnorm(0, 0.5),
                bM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model
res3 <- quap(model3, data=dat)
precis(res3, prob=0.95)

plot(coeftab(res1, res2, res3), pars=c("bM","bN"))
