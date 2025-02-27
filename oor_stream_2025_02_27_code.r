############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-02-27
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.1 - ?
#
# last updated: 2025-02-27

############################################################################

### 5.1: Spurious association

# load the rethinking package
library(rethinking)

# get the WaffleDivorce dataset and put it into 'dat'
dat <- get(data(WaffleDivorce))

# Figure 5.2
par(mfrow=c(1,2))
plot(Divorce ~ Marriage, data=dat, pch=21, bg="gray", bty="l",
     xlab="Marriage rate", ylab="Divorce rate")
plot(Divorce ~ MedianAgeMarriage, data=dat, pch=21, bg="gray", bty="l",
     xlab="Median age marriage", ylab="Divorce rate")
par(mfrow=c(1,1))

# standardize some variables
dat$D <- c(scale(dat$Divorce))
dat$M <- c(scale(dat$Marriage))
dat$A <- c(scale(dat$MedianAgeMarriage))

# SD of the MedianAgeMarriage variable
sd(dat$MedianAgeMarriage)

# define the regression model (predicting the standardized divorce rate from
# the standardized median age at marriage)
model <- alist(D ~ dnorm(mu, sigma),
               mu <- a + bA * A,
               a ~ dnorm(0, 0.2),
               bA ~ dnorm(0, 0.5),
               sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res <- quap(model, data=dat)
res

# sample 1000 values from the prior distributions
set.seed(10)
prior <- as.data.frame(extract.prior(res))
head(prior)

# plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l")
apply(prior[1:50,], 1, function(x) abline(x[1], x[2], lwd=1.2, col="gray30"))

