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

## Background

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
     panel.first=abline(0,1), xlim=c(0,1), ylim=c(0,1), main="Raw Data")

## Data issues

# Figure 10.6b: same plot as above, but using the adjusted data as described
# in the book (either .25 or .75 for uncontested elections)
plot(jitter(dat$v86_adj, amount=.01), jitter(dat$v88_adj, amount=.01),
     pch=c(4,1,19)[as.numeric(factor(dat$inc88))],
     xlab="Adjusted Dem. vote share in 1986", ylab="Adjusted Dem. vote share in 1988",
     panel.first=abline(0,1), xlim=c(0,1), ylim=c(0,1), main="Adjusted Data")

## Fitting the model

# load the rstanarm package
library(rstanarm)

# fit the regression model
set.seed(1234)
dat88 <- data.frame(vote=dat$v88_adj, past_vote=dat$v86_adj, inc=dat$inc88)
res <- stan_glm(vote ~ past_vote + inc, data=dat88, refresh=0)
print(res, digits=2)

## Simulation for inferences and predictions of new data points

# extract the sampled values from the posteriors distributions of the parameters
sims88 <- as.matrix(res)
head(sims88)

# histogram of sampled values for each parameter
par(mfrow=c(2,2))
hist(sims88[,1], main=colnames(sims88)[1], breaks=30, xlab="")
hist(sims88[,2], main=colnames(sims88)[2], breaks=30, xlab="")
hist(sims88[,3], main=colnames(sims88)[3], breaks=30, xlab="")
hist(sims88[,4], main=colnames(sims88)[4], breaks=30, xlab="")
par(mfrow=c(1,1))

# summary statistics for the sampled values for each parameter
round(apply(sims88, 2, median), digits=3)
round(apply(sims88, 2, mean), digits=3)
round(apply(sims88, 2, sd), digits=3)

# based on the model fitted above, predict the vote in 1990 based on the vote
# in 1988 using the 4000 sampled parameter values
dat90 <- data.frame(past_vote=dat$v88_adj, inc=dat$inc90)
pred90 <- posterior_predict(res, newdata=dat90)

# so we get a 4000x435 matrix
dim(pred90)

## Predictive simulation for a nonlinear function of new data

# determine for how many of the 435 districts was the vote share for the
# Democratic party is above 50% (based on each of the 4000 sampled values)
dems_pred <- rowSums(pred90 > 0.5)

# compute summary statistics for this count
mean(dems_pred)
median(dems_pred)
sd(dems_pred)