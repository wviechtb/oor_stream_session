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

# create the dataset for the example and examine it (the n values are based on
# guestimates of how many people fell into the various categories)
dat <- structure(list(x = c(-2, -1, 0, 1, 2), y = c(50, 44, 50, 47, 56),
                      n = c(300, 600, 1200, 600, 300)),
                 row.names = c(NA, -5L), class = "data.frame")
dat

# percentage of girls of parents in the lower attactiveness categories versus
# the highest attractiveness category
p1 <- weighted.mean(dat$y[1:4], dat$n[1:4])
p2 <- dat$y[5]
p1
p2

# difference between the two percentages
p2 - p1

# about 90% of the 3000 participants of the survey fell into the first four
# categories and 10% in the highest category
n1 <- 90/100 * 3000
n2 <- 10/100 * 3000

# standard error of the difference between the two percentages
se <- sqrt(p1 * (100 - p1) / n1 + p2 * (100 - p2) / n2)
se

# mean and SE for the prior (for the difference between the two percentages)
theta_hat_prior <- 0
se_prior <- 0.25

# combine the prior with the data using equation (9.3) and (9.4)
theta_hat_data <- p2 - p1
se_data <- se
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) / (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))
round(theta_hat_bayes, 2)
round(se_bayes, 2)

# plot the distributions for the prior, data, and posterior
xs <- seq(-2, 16, length=1000)
plot(xs, dnorm(xs, mean=theta_hat_prior, sd=se_prior), type="l", lty="dotted",
     bty="n", yaxt="n", yaxs="i", xlab=expression(theta), ylab="", ylim=c(0,1.6))
lines(xs, dnorm(xs, mean=theta_hat_data, sd=se_data), lty="dashed")
lines(xs, dnorm(xs, mean=theta_hat_bayes, sd=se_bayes))
legend("topright", lty=c("dotted", "dashed", "solid"),
       legend=c("Prior", "Data", "Posterior"))

# this plot demonstrates how little information there is in the actual data,
# relative to the prior information we have

# create a dataset with raw data like the survey dataset
dat <- lapply(split(dat, dat$x), function(x) cbind(attractiveness = rep(x$x, x$n),
                                                   girl = rep(c(0,1), times=round(c(1-x$y/100, x$y/100) * x$n))))
dat <- do.call(rbind, dat)
dat <- data.frame(dat)

# check that the counts are as expected
table(dat$attractiveness, dat$girl)

# or compute percentages of the counts across rows
prop.table(table(dat$attractiveness, dat$girl), margin=1) * 100

# construct a high versus low attractiveness dummy variable
dat$attracthigh <- ifelse(dat$attractiveness == 2, 1, 0)

# check that we get an estimated difference (between the proportions) of .08
# for the high versus not high attractiveness categories based on a standard
# linear regression model (using a 'linear probability model' to be precise)
res <- lm(girl ~ attracthigh, data=dat)
summary(res)

library(rstanarm)

# now we fit the same model using stan_glm(), specifying appropriate priors
# for the intercept and slope (using the default prior for the error SD)
set.seed(1241)
res <- stan_glm(girl ~ attracthigh, data=dat,
                prior_intercept=normal(location=0.49, scale=0.02),
                prior=normal(location=0, scale=0.0025), refresh=0)
res

# extract the median of the posterior for the intercept and slope and multiple
# by 100 to turn these values into percentages
coef(res) * 100

# compare the value for the slope to the value we calculated above manually
theta_hat_bayes

# extract the sampled values for the posterior of the intercept, slope, and sigma
post <- as.data.frame(res)

# plot the posterior distribution for the slope
plot(density(post[,2]))

