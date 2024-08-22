############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-08-22
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 10.1 - ?
#
# last updated: 2024-08-22

############################################################################

### 10.1: Adding predictors to a model

## Starting with a binary predictor

# download the dataset (need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# load the rstanarm package
library(rstanarm)

# fit linear regression model predicting the kids' test score from the dummy
# variable mom_hs (1/0 = mom did or did not graduate from high-school)
set.seed(1234)
res <- stan_glm(kid_score ~ mom_hs, data=dat, refresh=0)
res

# Figure 10.1
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_hs, amount=0.05), data=dat, pch=19, xlim=c(-0.2, 1.2),
     xlab="Mother completed high school", xaxt="n", ylab="Child test score", cex=0.5)
axis(side=1, at=c(0,1), labels=c("No (0)", "Yes (1)"))
abline(res, lwd=4)

# compute the observed means of the two groups
means <- with(dat, by(kid_score, mom_hs, mean))
means

# the slope of the regression model is really the difference between the two groups
means[2] - means[1]

# also add the observed means of the two groups as red points to the figure
points(c(0,1), means, pch=19, cex=2, col="red")

## A single continuous predictor

# fit linear regression model predicting the kids' test score from the mothers' IQ
res <- stan_glm(kid_score ~ mom_iq, data=dat, refresh=0)
res

# Figure 10.2
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_iq, amount=0.5), data=dat, pch=19,
     xlab="Mother IQ score", ylab="Child test score", cex=0.5)
abline(res, lwd=4)

diff.kids <- outer(dat$kid_score, dat$kid_score, "-")
diff.moms <- outer(dat$mom_iq,    dat$mom_iq, "-")

diff.kids[1:10,1:10]
diff.moms[1:10,1:10]

ratios <- (diff.kids / diff.moms)
ratios[1:10, 1:10]

ratios <- ratios[upper.tri(ratios)]
ratios
mean(ratios[!is.infinite(ratios)], na.rm=TRUE)

reldiff <- rep(NA_real_, 1000)
for (i in 1:1000) {
   sub <- dat[sample(nrow(dat), 2),]
   reldiff[i] <- (sub$kid_score[1] - sub$kid_score[2]) / (sub$mom_iq[1] - sub$mom_iq[2])
}

mean(reldiff[!is.infinite(reldiff)], na.rm=TRUE)
