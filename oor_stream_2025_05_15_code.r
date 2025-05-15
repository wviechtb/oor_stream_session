############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-15
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.4 - ?
#
# last updated: 2025-05-15

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.4: Latent-data formulation

# see Wikipedia for some details about the logistic distribution:
# https://en.wikipedia.org/wiki/Logistic_distribution
#
# in particular, note that the variance of a random variable following a
# logistic distribution is equal to s^2 * pi^2 / 3, where s is the 'scale'
# parameter

# Figure 13.5: plot of the pdf from a logistic distribution
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=0, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")

