############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-22
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 7.1 - ?
#
# last updated: 2025-05-22

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 7.1: The problem with parameters

## 7.1.1: More parameters (almost) always improve fit

# create the small dataset
dat <- data.frame(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"),
                  pos     = c(4, 3, 3, 4, 4, 3, 1),
                  brain   = c(438, 452, 612, 521, 752, 871, 1350),
                  mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))

# Figure 7.2: brain volume versus body size
plot(brain ~ mass, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
     ylab="brain volume (cc)", bty="l", xlim=c(30,70))
text(dat$mass, dat$brain, dat$species, pos=dat$pos)

# standardize body bass and rescale brain volumne so it goes from 0 to 1
dat <- transform(dat, mass_std  = (mass - mean(mass)) / sd(mass),
                      brain_std = brain / max(brain))

# fit the linear model
res1 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b*mass_std,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat)
precis(res1, digits=3)

# compare the results to using lm() (they are essentially the same)
res1.lm <- lm(brain_std ~ mass_std, data=dat)
summary(res1.lm)
sigma(res1.lm)

# sidenote: to compare sigma to the Bayesian model we should extract samples
# from the posterior distributions, including the one for log(sigma), then
# transform the latter to sigma, and then take the mean
post <- extract.samples(res1)
head(post)
mean(exp(post$log_sigma))

# can also take samples for the intercept and slope based on the lm() model (but not sigma)
post.lm <- extract.samples(res1.lm)
head(post.lm)

# simulate posterior observations of brain_std corresponding to the 7 observed species
set.seed(12)
s <- sim(res1)
head(s)

# compute the residuals
r <- apply(s, 2, mean) - dat$brain_std

# compute R^2 (note: using variances computed with n in the denominator, not n-1)
resid_var <- var2(r)
outcome_var <- var2(dat$brain_std)
1 - resid_var/outcome_var

# function to compute R^2 in this manner
R2_is_bad <- function(quap_fit) {
   s <- sim(quap_fit, refresh=0)
   r <- apply(s, 2, mean) - dat$brain_std
   1 - var2(r) / var2(dat$brain_std)
}

# try this out (using the same seed as above to check that we get the same result)
set.seed(12)
R2_is_bad(res1)

# fit polynomial models of degree 2 to 6
res2 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,2)))
res3 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,3)))
res4 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,4)))
res5 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,5)))
res6 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5 + b[6]*mass_std^6,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,6)))

# note: model res6 also runs; we do not need to fix exp(log_sigma) to 0.001

# reminder:
# - sim() simulates posterior observations of y|x (i.e., for single individuals)
# - link() simulates posterior predictions of E[y|x]

# simulates posterior predictions for mass_std values between min and max mass_std
mass_seq <- seq(from=min(dat$mass_std), to=max(dat$mass_std), length.out=100)
l <- link(res1, data=list(mass_std=mass_seq))
head(l)

# compute the mean and percentile interval for each column
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)

# plot the data and add the line based on the mean with the corresponding interval
plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="standardized body mass (kg)",
     ylab="standardized brain volume (cc)", bty="l")
lines(mass_seq, mu)
shade(ci, mass_seq)

# Figure 7.3: corresponding plots for all 6 models
par(mfrow=c(3,2))
res <- list(res1, res2, res3, res4, res5, res6)
mass_seq <- seq(from=-1.2, to=1.5, length.out=100)
xs <- seq(30, 65, by=5)
xs_std <- (xs-mean(dat$mass))/sd(dat$mass)
ys <- seq(0, 1800, by=450)
ys_std <- ys / max(dat$brain)
invisible(lapply(res, function(x) {
   l <- link(x, data=list(mass_std=mass_seq))
   mu <- apply(l, 2, mean)
   ci <- apply(l, 2, PI)
   plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
        ylab="brain volume (cc)", bty="l", xlim=range(xs_std),
        ylim=c(min(ci,brain_std),max(ci,brain_std)), xaxt="n", yaxt="n")
   axis(side=1, at=xs_std, labels=xs)
   axis(side=2, at=ys_std, labels=ys)
   lines(mass_seq, mu, lwd=3)
   shade(ci, mass_seq)
   mtext(paste0("R^2 = ", round(R2_is_bad(x), digits=2)), cex=0.8)
}))
par(mfrow=c(1,1))

## 7.1.2: Too few parameters hurts, too

# Figure 7.4 (left): regression lines based on the linear model leaving out
# one data point at a time
plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
     ylab="brain volume (cc)", bty="l", xlim=range(xs_std),
     ylim=range(brain_std), xaxt="n", yaxt="n")
axis(side=1, at=xs_std, labels=xs)
axis(side=2, at=ys_std, labels=ys)

invisible(lapply(1:7, function(i) {
   tmp <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                      mu <- a + b*mass_std,
                      a ~ dnorm(0.5, 1),
                      b ~ dnorm(0, 10),
                      log_sigma ~ dnorm(0, 1)), data=dat[-i,])
   l <- link(tmp, data=list(mass_std=mass_seq))
   mu <- apply(l, 2, mean)
   lines(mass_seq, mu, lwd=3, col="gray40")
}))

# Figure 7.4 (right): regression lines based on the 4th degree polynomial
# model leaving out one data point at a time
plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
     ylab="brain volume (cc)", bty="l", xlim=range(xs_std),
     ylim=c(-200,2000)/max(dat$brain), xaxt="n", yaxt="n")
axis(side=1, at=xs_std, labels=xs)
axis(side=2, at=ys_std, labels=ys)

invisible(lapply(1:7, function(i) {
   tmp <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                     mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4,
                     a ~ dnorm(0.5, 1),
                     b ~ dnorm(0, 10),
                     log_sigma ~ dnorm(0, 1)), data=dat[-i,], start=list(b=rep(0,4)))
   l <- link(tmp, data=list(mass_std=mass_seq))
   mu <- apply(l, 2, mean)
   lines(mass_seq, mu, lwd=3, col="gray40")
}))

############################################################################

### 7.2: Entropy and accuracy
