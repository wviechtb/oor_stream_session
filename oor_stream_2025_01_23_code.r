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
dat$weight.s <- c(scale(dat$weight))
