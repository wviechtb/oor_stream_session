############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-30
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 9.4 - ?
#
# last updated: 2024-05-30

############################################################################

### 9.4: Example of Bayesian inference: beauty and sex ratio

# create the dataset for the example and examine it
dat <- structure(list(x = c(-2, -1, 0, 1, 2), y = c(50, 44, 50, 47, 56)),
                 row.names = c(NA, -5L), class = "data.frame")
dat

# proportion of girls of parents in the lower attactiveness categories versus
# the highest attractiveness category
p1 <- mean(dat$y[1:4])
p2 <- dat$y[5]
p1
p2

# assume that 4/5th of the respondents are in the first group and the other
# 1/5th in the other group
n1 <- 4/5 * 3000
n2 <- 1/5 * 3000

