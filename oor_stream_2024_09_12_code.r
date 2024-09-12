############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-12
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 10.6 - ?
#
# last updated: 2024-09-12

############################################################################

### 10.6: Example: uncertainty in predicting congressional elections

# download the data for the example
if (!file.exists("congress.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/congress.csv", destfile="congress.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("congress.csv")
head(dat)

# treat proportions for v88 below 10% or above 90% as 'uncontested' vote
# shares (i.e., change the proportions to essentially 0 or 1, respectively)
dat$v88_hist <- ifelse(dat$v88<.1, .0001, ifelse(dat$v88>.9, .9999, dat$v88))

# Figure 10.5: proportion of the vote share for the Democratic party in the
# 435 congressional districts in 1988 (using the v88_hist variable)
hist(dat$v88_hist, breaks=seq(0,1,by=.05), main="",
     xlab="Democratic share of the two-party vote")

# Figure 10.6a: proportion of the vote share in 1988 versus 1986 with filled
# circles when the incumbent is from the Democratic party, crosses when the
# incumbent is from the Republican party, and unfilled circles otherwise
plot(jitter(dat$v86, amount=.01), jitter(dat$v88, amount=.01),
     pch=c(4,1,19)[as.numeric(factor(dat$inc88))],
     xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988",
     panel.first=abline(0,1))

# load the rstanarm package
library(rstanarm)

set.seed(1236)
res <- stan_glm(v88 ~ v86 + inc88, data=dat, refresh=0)
print(res, digits=1)

