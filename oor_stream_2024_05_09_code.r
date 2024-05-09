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
# how much we change the y value, b is not affected; for points where h is
# large, changes in y have a bigger impact on the slope

# create a dataset like in Figure 8.3
dat <- data.frame(x = 2:12, y = c(11,1,12,11,8,24,19,25,11,28,19))
dat

# plot the data
plot(dat$x, dat$y, pch=19, bty="l", panel.first=grid(),
     xlab="x", ylab="y", ylim=c(0,30))

# add the regression line
res <- lm(y ~ x, data=dat)
abline(res, lwd=3)

# add lines extending from the regression line to each observed value
segments(dat$x, fitted(res), dat$x, dat$y)

# compute the h values and examine them
h <- with(dat, (x - mean(x)) / sum((x - mean(x))^2))
h

# show that the slope is a linear combination of the y values
coef(res)[[2]]
sum(h * dat$y)

# show that the slope is unaffected when the y value for the 6th point (for
# which x = mean(x)) is changed
dat$y[6] <- 5
points(dat$x[6], dat$y[6], pch=21)
res <- lm(y ~ x, data=dat)
abline(res, lwd=3, lty="dotted")

# change the y value back to the original one for the 6th point
dat$y[6] <- 24

# show that the slope becomes steeper when y value for the 11th point (for
# which h > 0) is increased
dat$y[11] <- 30
points(dat$x[11], dat$y[11], pch=21)
res <- lm(y ~ x, data=dat)
abline(res, lwd=3, lty="dashed")

############################################################################

### 8.3: Least squares slope as a weighted average of slopes of pairs

############################################################################


# first simulate the same data as we did last time
set.seed(1239)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

