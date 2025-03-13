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
model <- alist(K ~ dnorm(mu, sigma),
               mu <- a + bN*N,
               a ~ dnorm(0, 1),
               bN ~ dnorm(0, 1),
               sigma ~ dexp(1))

# fit the model
res <- quap(model, data=dat)

# get an error message, due to the missing values in neocortex.perc (and hence N)
dat$neocortex.perc

# keep rows where K, N, and M are complete (i.e., not missing)
dat <- dat[complete.cases(dat$K,dat$N,dat$M),]
dat

# fit the model using the complete data
res <- quap(model, data=dat)

# sample 1000 values from the prior distributions
prior <- data.frame(extract.prior(res))
head(prior)

# plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortext Percent (std)",
     ylab="Kilocal per g (std)", bty="l")
invisible(apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.5, col="gray30")))




precis(res, prob=0.95)

