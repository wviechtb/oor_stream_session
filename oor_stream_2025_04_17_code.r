############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.1 - ?
#
# last updated: 2025-04-17

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.1: Logistic regression with a single predictor

# make copies of the qlogis and plogis functions with easier to remember names
logit <- qlogis
invlogit <- plogis

# Figure 13.1(a):
xs <- seq(-6, 6, length.out=1000)
ys <- invlogit(xs)
par(mar=c(5,5,4,2), xaxs="i", yaxs="i")
plot(xs, ys, type="l", bty="l", xlab="x", ylab=expression("logit"^-1 * (x)), ylim=c(0,1))
title(expression(y == logit^-1 * (x)))
segments(0, 0, 0, invlogit(0), lty="dotted")
segments(-6, invlogit(0), 0, invlogit(0), lty="dotted")

## Example: modeling political preference given income

# download the dataset if it doesn't already exist
if (!file.exists("nes.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/NES/data/nes.txt", destfile="nes.txt")

# read in the dataset
dat <- read.table("nes.txt", header=TRUE)

# inspect the first six rows of the dataset
head(dat)

# keep only the data from 1992 and exclude respondents who preferred other candidates or had no opinion
ok <- dat$year==1992 & !is.na(nes$rvote) & !is.na(nes$dvote) & (nes$rvote==1 | nes$dvote==1)
nes92 <- nes[ok,]


plot(jitter(rvote, amount=0.05) ~ jitter(income, amount=.05), data=dat, bty="l", pch=19, cex=0.2)