############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-26
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.1 - ?
#
# last updated: 2024-09-26

############################################################################

### 11.1: Assumptions of regression analysis

# although normality of errors is the least important assumption discussed
# here, let's reiterate the point made that checking the distribution of y
# itself is not relevant using the example discussed on page 155

x <- sample(c(0,1,2), size=10000, replace=TRUE)
y <- 0.2 + 0.5*x + rnorm(10000, mean=0, sd=0.1)
hist(y, breaks=100, main="Distribution of y")

# obviously, the distribution of y itself is not normal; but the distribution
# of the errors is
e <- y - (0.2 + 0.5*x)
hist(e, breaks=100, main="Distribution of the errors")

############################################################################

### 11.2: Plotting the data and fitted model

## Displaying a regression line as a function of one input variable

# download the dataset (need to do this once)
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# load the rstanarm package
library(rstanarm)

# fit a linear regression model predicting the kids' test score from the moms' IQ
res <- stan_glm(kid_score ~ mom_iq, data=dat, refresh=0)
res

# plot the data and add the regression line
plot(dat$mom_iq, dat$kid_score, xlab="Mother IQ score",
     ylab="Child test score", pch=21, bg="darkgray")
#abline(coef(res)[1], coef(res)[2], lwd=5)
abline(res, lwd=5) # same thing for a model with a single predictor

## Displaying two fitted regression lines

# add mom_hs (0/1 for whether a mom completed high-school) as a predictor
res <- stan_glm(kid_score ~ mom_hs + mom_iq, data=dat, refresh=0)
res

# plot the data and add the regression lines for the two groups created by mom_hs
colors <- ifelse(kidiq$mom_hs==1, "darkgray", "black") # note: colors switched
plot(dat$mom_iq, dat$kid_score, xlab="Mother IQ score",
     ylab="Child test score", pch=20, col=colors, cex=1.4)
b_hat <- coef(res)
b_hat
abline(b_hat[1], b_hat[3],            col="black", lwd=5)
abline(b_hat[1] + b_hat[2], b_hat[3], col="darkgray", lwd=5)

# an example where the second predictor is continuous
res <- stan_glm(kid_score ~ mom_age + mom_iq, data=dat, refresh=0)
res

# plot the data and add the regression line when mom_age=17 and when mom_age=29
plot(dat$mom_iq, dat$kid_score, xlab="Mother IQ score",
     ylab="Child test score", pch=21, bg="darkgray")
b_hat <- coef(res)
b_hat
abline(b_hat[1] + b_hat[2]*17, b_hat[3], lwd=5)
abline(b_hat[1] + b_hat[2]*29, b_hat[3], lwd=5)

# above, we used the minimum and maximum mom_age values observed to draw the lines
range(dat$mom_age)

# often, people use mean +- one standard deviation
round(with(dat, mean(mom_age) + c(-1,1)*sd(mom_age)), digits=1)

# now let's go back to the model with mom_hs and mom_iq but with the interaction
res <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=dat, refresh=0)
colors <- ifelse(kidiq$mom_hs==1, "darkgray", "black") # note: colors switched
plot(dat$mom_iq, dat$kid_score, xlab="Mother IQ score",
     ylab="Child test score", pch=20, col=colors, cex=1.4)
b_hat <- coef(res)
b_hat
abline(b_hat[1],            b_hat[3],            col="black", lwd=5)
abline(b_hat[1] + b_hat[2], b_hat[3] + b_hat[4], col="darkgray", lwd=5)

## Displaying uncertainty in the fitted regression

# let's go back to the simple regression model
res <- stan_glm(kid_score ~ mom_iq, data=dat, refresh=0)
res

# save the sampled values from the posterior distributions
sims <- as.data.frame(res)
head(sims)
dim(sims)

# draw a histogram for the sampled values for the slope
hist(sims$mom_iq, breaks=50, main="Histogram of samples from the posterior distribution of the slope")

# note: coef(res) gives us the median of the sampled values
coef(res)
apply(sims, 2, median)

# plot the data again
plot(dat$mom_iq, dat$kid_score, xlab="Mother IQ score",
     ylab="Child test score", pch=21, bg="darkgray")

# add the regression lines based on the posterior samples of the intercept and
# slope and use 'alpha blending' so that darker regions reflect more commonly
# observed intercept and slope combinations from the posteriors (so instead of
# drawing 10 randomly chosen lines as was done in the book in Figure 11.1, we
# show all of them)
apply(sims, 1, function(x) abline(x[1], x[2], col=rgb(0,0,0,.02)))

# add the line based on the median intercept and slope values
abline(res, col="red", lwd=5)

## Displaying using one plot for each input variable

# back to the model where we add mom_hs as a predictor
res <- stan_glm(kid_score ~ mom_hs + mom_iq, data=dat, refresh=0)
res

# save the median of the samples values
b_hat <- coef(res)

# save the sampled values from the posterior distributions
sims <- as.data.frame(res)

par(mfrow=c(1,2))

# plot kid_score versus mom_iq (as before)
plot(dat$mom_iq, dat$kid_score, xlab="Mother IQ score",
     ylab="Child test score", pch=21, bg="darkgray")

# compute the mean of mom_hs
mean_mom_hs <- mean(dat$mom_hs)

# now we again add the regression lines based on the posterior samples of the
# regression coefficients holding the value of mom_hs constant at its mean
apply(sims, 1, function(x) abline(x[1] + x[2]*mean_mom_hs, x[3], col=rgb(0,0,0,.01)))

# add the line based on the median values
abline(b_hat[1] + b_hat[2]*mean_mom_hs, b_hat[3], col="red", lwd=5)

# plot kid_score versus mom_hs
plot(jitter(dat$mom_hs, amount=.05), dat$kid_score,
     xlab="Mother completed high school", ylab="Child test score",
     pch=21, bg="darkgray", xaxt="n")
axis(side=1, at=c(0,1))

# compute the mean of mom_iq
mean_mom_iq <- mean(dat$mom_iq)

# now we again add the regression lines based on the posterior samples of the
# regression coefficients holding the value of mom_iq constant at its mean
apply(sims, 1, function(x) abline(x[1] + x[3]*mean_mom_iq, x[2], col=rgb(0,0,0,.01)))

# add the line based on the median values
abline(b_hat[1] + b_hat[3]*mean_mom_iq, b_hat[2], col="red", lwd=5)

par(mfrow=c(1,2))

## Plotting the outcome vs. a continuous predictor

# simulate data based on the model described
set.seed(1236)
N <- 100
x <- runif(N, 0, 1)
z <- sample(c(0,1), size=N, replace=TRUE)
a <- 1
b <- 2
theta <- 5
sigma <- 2
y <- a + b*x + theta*z + rnorm(N, mean=0, sd=sigma)
dat <- data.frame(x=x, y=y, z=z)
rm(x,y,z)

# fit the model based on the simulated data
res <- stan_glm(y ~ x + z, data=dat, refresh=0)
res

# Figure 11.3

par(mfrow=c(1,2))

for (i in 0:1) {
   plot(y ~ x, data=dat, xlim=range(x), ylim=range(y), subset=z==i, main=paste("z =", i),
        xlab="Pre-treatment indicator, x", ylab="Outcome, y", pch=20+i)
   abline(coef(res)[1] + coef(res)[3]*i, coef(res)[2], lwd=5)
}

par(mfrow=c(1,1))

## Forming a linear predictor from a multiple regression

# simulate data based on the model described
set.seed(1236)
N <- 100
K <- 10
X <- replicate(K, runif(N, 0, 1))
z <- sample(c(0,1), size=N, replace=TRUE)
a <- 1
b <- 1:K
theta <- 5
sigma <- 2
y <- a + X %*% b + theta*z + rnorm(N, mean=0, sd=sigma)
dat <- data.frame(X=X, y=y, z=z)
head(dat)

# fit the model
res <- stan_glm(y ~ X + z, data=dat, refresh=0)
