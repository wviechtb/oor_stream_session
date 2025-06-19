############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-06-19
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 7.3 - ?
#
# last updated: 2025-06-19

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 7.3: Golem taming: regularization

# Figure 7.7: plots of the density of a normal distribution with mean 0 and
# standard deviations of 1, 0.5, and 0.2
plot(NA, xlim=c(-3,3), ylim=c(0,2), xlab="parameter value", ylab="density",
     bty="l", las=1)
curve(dnorm(x, mean=0, sd=1),   from=-3, to=3, n=1001, add=TRUE, lty="dashed")
curve(dnorm(x, mean=0, sd=0.5), from=-3, to=3, n=1001, add=TRUE)
curve(dnorm(x, mean=0, sd=0.2), from=-3, to=3, n=1001, add=TRUE, lwd=3)
