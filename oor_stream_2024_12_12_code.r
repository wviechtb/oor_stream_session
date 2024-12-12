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
pred <- colMeans(pred)
head(pred)

# Figure 11.19: plot of the mean predicted value of each child against the
# corresponding actually observed value
plot(pred, dat.new$kid_score, pch=21, bg="gray", bty="l",
     xlab="Predicted score", ylab="Actual score", panel.first=abline(0,1,lwd=3),
     xlim=c(20,140), ylim=c(20,140))

# same figure but using more natural ranges for the x- and y-axis
plot(pred, dat.new$kid_score, pch=21, bg="gray", bty="l",
     xlab="Predicted score", ylab="Actual score")

# correlation between predicted and observed scores
cor(pred, dat.new$kid_score)


