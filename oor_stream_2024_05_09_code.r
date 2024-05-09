############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-09
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 8.2 - ?
#
# last updated: 2024-05-09

############################################################################

### 8.2 Influence of individual points in a fitted regression

# we can rewrite equation (8.3) as follows:
#
# b = sum(h * y),
#
# where h = (x - mean(x)) / sum((x - mean(x))^2)
#
# so we see that the slope is a linear combination of the y values and if we
# change a particular value of y, then the slope is change accordingly, but
# how much it changes depends on the corresponding h value
#
# if the corresponding x value is equal to mean(x), then h = 0, and no matter
# how much we change the y value, b is not affected

# first simulate the same data as we did last time
set.seed(1239)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

