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

# or examine the entire posterior distribution for this count (note: since the
# outcome is a count, instead of a histogram, we make a barplot of the
# frequencies of the counts between the minimum and maximum)
barplot(table(factor(dems_pred, levels=min(dems_pred):max(dems_pred))),
        xlab="Number of Districts Won", ylab="Frequency")

# we could fit the same model using 'regular' regression and also compute
# predicted values for the 1990 election based on this model and then count
# how many of those predicted values are larger than 50%
res.lm <- lm(vote ~ past_vote + inc, data=dat88)
pred90.lm <- predict(res.lm, newdata=dat90)
sum(pred90.lm > 0.5)

# we get essentially the same answer as above, but it would difficult to
# derive a standard error for this value; using the Bayesian approach we used
# above, we automatically get a measure of the uncertainty in this estimate

## Combining simulation and analytic calculations

# suppose there are 1000 people in district 147
n147 <- 1000

# then based on the predicted values for the vote share in that district, we
# can compute the chances of a tied vote (500 for the Democratic candidate,
# 500 for the Republican one) as follows

# number of cases where the vote is not tied versus tied
table(round(pred90[,147] * n147) == n147/2)

# proportion of cases where the vote is tied
proptie <- prop.table(table(round(pred90[,147] * n147) == n147/2))[2]
proptie

# now suppose there are 50 people in district 147; then we get
n147 <- 50
proptie <- prop.table(table(round(pred90[,147] * n147) == n147/2))[2]
proptie

# note: with 50 people, a vote share just above 0.49 to just below 0.51 is
# undecided, since 0.49*50 = 24.5 and 0.51*50 = 25.5

# based on this, we can compute the chances of a tied vote if the district
# actually had 1000 people with
proptie / (.02 * 1000)

# the reason why this differs from what we obtained above is sampling
# uncertainty; these two would be very similar to each other if had sampled
# more values from the posterior

# however, the number of people in districts is more around 750,000
n147 <- 750000
table(round(pred90[,147] * n147) == n147/2)

# then we start running into the problem that a tie never happens in the
# sampled values; we could increase the number of sampled values, but this
# becomes computationally very demanding; instead, we can use our observation
# above and estimate the chances of a tied vote with this
proptie / (.02 * 750000)

# or let's pretend there are 10 people in the district (note then that a vote
# share just above 0.45 to just below 0.55 is undecided, since 0.45*10 = 4.5
# and 0.55*10 = 5.5); then we can do the estimation of a tied vote as follows
n147 <- 10
proptie <- prop.table(table(round(pred90[,147] * n147) == n147/2))[2]
proptie / (.1 * 750000)

# instead, we could assume the proportion of vote shares for the district are
# normally distributed, with the mean and sd we obtain from the sampled values
mean147 <- mean(pred90[,147])
sd147 <- sd(pred90[,147])
mean147
sd147

# then we want to know for this distribution what area falls within this interval
0.5 - 1 / (2*750000)
0.5 + 1 / (2*750000)

# we can obtain this as follows, which gives us a very similar estimate for
# the chances of a tied vote as we obtained above
pnorm(0.5 + 1 / (2*750000), mean=mean147, sd=sd147, lower.tail=TRUE) -
pnorm(0.5 - 1 / (2*750000), mean=mean147, sd=sd147, lower.tail=TRUE)

# note: since n=435 here, whether we use a normal or t-distribution for this
# calculations makes essentially no difference

# if we would use a t-distribution, then we get essentially the same answer
pt((0.5 + 1 / (2*750000) - mean147) / sd147, df=435-3, lower.tail=TRUE) -
pt((0.5 - 1 / (2*750000) - mean147) / sd147, df=435-3, lower.tail=TRUE)
