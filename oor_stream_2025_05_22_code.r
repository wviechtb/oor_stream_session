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
res1 <- quap(alist(brain_std ~ dnorm(mu, sigma),
                   mu <- a + b*mass_std,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1),
                   sigma <- exp(log_sigma)), data=dat)
precis(res1, digits=3)

# compare the results to using lm() (they are essentially the same)
res1.lm <- lm(brain_std ~ mass_std, data=dat)
summary(res1.lm)
sigma(res1.lm)

# sidenote: to compare
post <- extract.samples(res1)
exp(mean(post$log_sigma))
mean(exp(post$log_sigma))