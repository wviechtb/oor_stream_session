############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-12-12
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.7
#
# last updated: 2024-12-12

############################################################################

### 11.7: External validation: checking fitted model on new data

# load the rstanarm package
library(rstanarm)

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# the dataset doesn't actually include information about the age of the
# children, so we cannot replicate exactly what is described in the book;
# however, let's pretend that the first 98 rows of the dataset contains the
# data of the older children, so we fit the model based on this subset
res1 <- stan_glm(kid_score ~ mom_iq + mom_hs, data=dat, refresh=0, subset=1:98)

# compute predicted values for the other children
dat.new <- dat[-(1:98),]
pred <- posterior_predict(res1, newdata=dat.new)

# note: pred contains 4000 predicted values for each child, based on the 4000
# sampled values of the intercept and slope from the posterior distributions;
# compute the mean predicted value of each child
meanpred <- colMeans(pred)
head(meanpred)

# Figure 11.19(a): plot of the mean predicted value of each child against the
# corresponding actually observed value
plot(meanpred, dat.new$kid_score, pch=21, bg="gray", bty="l",
     xlab="Predicted score", ylab="Actual score", panel.first=abline(0,1,lwd=3),
     xlim=c(20,140), ylim=c(20,140))

# same figure but using more natural ranges for the x- and y-axis
plot(meanpred, dat.new$kid_score, pch=21, bg="gray", bty="l",
     xlab="Predicted score", ylab="Actual score")

# correlation between predicted and observed scores
cor(meanpred, dat.new$kid_score)

# compute the residuals
resid <- dat.new$kid_score - meanpred

# Figure 11.19(b): plot of the predicted values versus the residuals
plot(meanpred, resid, pch=21, bg="gray", bty="l", xlab="Predicted score",
     ylab="Prediction error", panel.first={abline(h=0, lwd=3);
     abline(h=c(-1,1) * sd(resid), lty="dashed")})

# compute 50% predictive intervals for each child
predint <- apply(pred, 2, function(x) quantile(x, prob=c(.25, .75)))
predint[,1:5]

# compute how often the actual score is within the predictive interval
mean(dat.new$kid_score >= predint[1,] & dat.new$kid_score <= predint[2,])

############################################################################

## 11.8: Cross validation

# simulate 20 data points base on a simple regression model
n <- 20
x <- 1:n
a <- 0.2
b <- 0.3
sigma <- 1
set.seed(2141)
y <- a + b*x + rnorm(n, mean=0, sd=sigma)
dat <- data.frame(x, y)

# Figure 11.20: plot of the data
plot(y ~ x, data=dat, pch=21, bg="gray", bty="l", xlim=c(0,20), ylim=c(0,8))

# fit the model to all data and to the data leaving out the 18th data point
res.all <- stan_glm(y ~ x, data=dat, refresh=0)
res.m18 <- stan_glm(y ~ x, data=dat[-18,], refresh=0)

# add the regression lines from these models to the plot
abline(res.all, lwd=3)
abline(res.m18, lwd=3, lty="dashed")

# compute predicted values for the 18th data point based on the model with all
# data and the model where we left out the 18th data point
pred.all <- posterior_predict(res.all, newdata=dat[18,])
pred.m18 <- posterior_predict(res.m18, newdata=dat[18,])

pred.all