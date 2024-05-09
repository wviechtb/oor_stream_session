############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-09
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 8.2
#
# last updated: 2024-05-09

############################################################################

### 8.2 Influence of individual points in a fitted regression

# first simulate the same data as we did last time
set.seed(1239)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

