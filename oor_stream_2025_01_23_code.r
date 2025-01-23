############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-01-23
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.5 - ?
#
# last updated: 2025-01-23

############################################################################

### 4.5: Curves from lines

# load the rethinking package
library(rethinking)

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

## 4.5.1: Polynomial regression

# plot the height of the individuals versus their weight
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")

# mu_i = alpha     + beta_1 x_i + beta_2 x^2_i
#      = alpha     + (beta_1 + beta_2 x_i) * x_i
#      = intercept + (slope)               * x_i
#
# so when x_i = 0, then beta_1 is the slope of the linear relationship between
# mu_i and x_i, but if x_i is not 0, then the slope is beta_1 + beta_2 x_i and
# hence the slope depends on x_i itself

# standardize weight (using the scale() function)
dat$weight.s  <- c(scale(dat$weight))

# compute the square of the standardized weight values
dat$weight.s2 <- dat$weight.s^2

# fit the quadratic polynomial regression model
model <- alist(height ~ dnorm(mu, sigma),
               mu <- a + b1*weight.s + b2*weight.s2,
               a  ~ dnorm(178, 20),
               b1 ~ dlnorm(0, 1),
               b2 ~ dnorm(0, 1),
               sigma ~ dunif(0, 50))
res <- quap(model, data=dat)
res
precis(res)

# compute the predicted mean height as a function of 30 (standardized) weight
# values between -2.2 and 2 (and the corresponding 95% compatibility intervals)
weight.seq <- seq(from=-2.2, to=2, length.out=30)
pred.dat   <- data.frame(weight.s=weight.seq, weight.s2=weight.seq^2)
mu         <- link(res, data=pred.dat)
mu.mean    <- apply(mu, 2, mean)
mu.PI      <- apply(mu, 2, PI, prob=0.95)

# compute corresponding 95% prediction intervals for the height of individuals
sim.height <- sim(res, data=pred.dat)
height.PI  <- apply(sim.height, 2, PI, prob=0.95)

# plot the data again and add the predicted means and intervals to the plot
plot(height ~ weight.s, data=dat, col=col.alpha(rangi2,0.5), pch=19)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# compute the cube of the standardized weight values
dat$weight.s3 <- dat$weight.s^3

# fit the cubic polynomial regression model
model <- alist(height ~ dnorm(mu, sigma),
               mu <- a + b1*weight.s + b2*weight.s2 + b3*weight.s3,
               a  ~ dnorm(178, 20),
               b1 ~ dlnorm(0, 1),
               b2 ~ dnorm(0, 1),
               b3 ~ dnorm(0, 1),
               sigma ~ dunif(0, 50))
res <- quap(model, data=dat)
res
precis(res)

# compute the predicted means and intervals again based on the cubic model
pred.dat   <- data.frame(weight.s=weight.seq, weight.s2=weight.seq^2, weight.s3=weight.seq^3)
mu         <- link(res, data=pred.dat)
mu.mean    <- apply(mu, 2, mean)
mu.PI      <- apply(mu, 2, PI, prob=0.95)
sim.height <- sim(res, data=pred.dat)
height.PI  <- apply(sim.height, 2, PI, prob=0.95)

# plot the data again and add the predicted means and intervals to the plot
plot(height ~ weight.s, data=dat, col=col.alpha(rangi2,0.5), pch=19)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# same plot as above, but show weight on the x-axis, not standardized weight
plot(height ~ weight.s, data=dat, col=col.alpha(rangi2,0.5), pch=19,
     xaxt="n", xlab="weight")
labels <- seq(5, 65, by=10)
at <- (labels - mean(dat$weight)) / sd(dat$weight)
axis(side=1, at=at, labels=round(labels,1))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

## 4.5.2: Splines

# load the cherry tree data and examine some summary statistics
dat <- get(data(cherry_blossoms))
precis(dat, prob=0.95)

