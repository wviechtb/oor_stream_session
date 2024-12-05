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

# Figure 4.5: plot the regression lines implied by these values
plot(NA, xlim=range(dat$weight), ylim=c(-100,400), xlab="weight", ylab="height", bty="l")
abline(h=0, lty=2)
abline(h=272, lty=1)
mtext("b ~ dnorm(0,10)")
xbar <- mean(dat$weight)
xs <- seq(min(dat$weight), max(dat$weight))
invisible(apply(sim, 1, function(par) lines(xs, par["a"] + par["b"] * (xs - xbar), col=col.alpha("black",0.2), lwd=2)))

