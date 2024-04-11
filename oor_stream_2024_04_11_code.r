############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-04-11
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 6.5 - ?
#
# last updated: 2024-04-11

############################################################################

### 6.5: The paradox of regression to the mean

# install the rstanarm package (need to do this once)
#install.packages("rstanarm")

# load the rstanarm package
library(rstanarm)

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/PearsonLee/data/Heights.txt", destfile="heights.txt")

# read in the data
dat <- read.table("heights.txt", header=TRUE)

# inspect the dataset
head(dat)

# Figure 6.3a: mother's height versus daughter's height (with jittering to
# avoid points that overlap)
plot(jitter(daughter_height, amount=0.5) ~ jitter(mother_height, amount=0.5),
     data=dat, pch=19, cex=0.2, xlab="Mother's height (inches)",
     ylab="Adult daughter's height (inches)", bty="l")
grid()

# fit a simple linear regression model predicting the daughter's height based
# on the mother's height
res <- stan_glm(daughter_height ~ mother_height, data=dat)
res

# extract the coefficients (rounded to two decimal places)
round(coef(res), digits=2)

# add the regression line to the plot
abline(res, lwd=5)

# also add a diagonal line with a slope of 1
abline(a=0, b=1, lwd=5, lty="dotted")

# compare the variance of mothers' and daughters' heights
var(dat$mother_height)
var(dat$daughter_height)

# so the variation in daughers' heights is still as large (or even a bit
# larger) than the variation in mothers' heights

############################################################################

set.seed(1234)

heights <- rnorm(5000, mean=62.5, sd=2.3)

generations <- 100

means <- rep(NA, generations)
sds   <- rep(NA, generations)

for (i in 1:generations) {
   means[i] <- mean(heights)
   sds[i]   <- sd(heights)
   heights <- 30 + 0.5 * heights + rnorm(5000, mean=0, sd=2.3)
}

plot(1:generations, means, type="o", pch=21, bg="gray")

# it can be shown that the means converge to intercept / (1 - slope)
30 / (1 - 0.5)
abline(h = 30 / (1 - 0.5), lty="dotted")

plot(1:generations, sds, type="o", pch=21, bg="gray")

# it can be shown that the SDs converge to sigma / sqrt(1 - slope^2)
2.3 / sqrt(1 - 0.5^2)
abline(h = 2.3 / sqrt(1 - 0.5^2), lty="dotted")

############################################################################

## How regression to the mean can confuse people about causal inference;
## demonstration using fake data

set.seed(1234)
ability <- rnorm(1000, mean=50, sd=10)
midterm <- ability + rnorm(1000, mean=0, sd=10)
final   <- ability + rnorm(1000, mean=0, sd=10)

dat <- data.frame(midterm, final)
rm(ability, midterm, final)

res <- stan_glm(final ~ midterm, data=dat)
res

plot(final ~ midterm, data=dat, pch=21, bg="gray",
     xlab="Midterm exam score", ylab="Final exam score")
grid()
abline(res, lwd=5)
abline(a=0, b=1, lwd=5, lty="dotted")

############################################################################

### 7.1: Example: predicting presidential vote share from the economy

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data
dat <- read.table("hibbs.dat", header=TRUE)

# look at the data
dat

# plot growth on the x-axis versus vote on the y-axis (Figure 7.2a)
plot(vote ~ growth, data=dat, xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", pch=NA, xaxt="n", yaxt="n",
     xlim=c(-0.5,4.5), ylim=c(43,62), bty="l")
abline(h=50, col="gray")
with(dat, text(growth, vote, year, pos=4))
axis(side=1, at=0:4, labels=paste0(0:4, "%"))
axis(side=2, at=c(45,50,55,60), labels=paste0(c(45,50,55,60), "%"))
title("Forecasting the election from the economy")

# fit the model using stan_glm() (first setting the seed of the random number
# generator to make the results fully reproducible)
set.seed(1237)
res <- stan_glm(vote ~ growth, data=dat)
res

# model fitting with stan_glm() involves some aspects that are random (to be
# discussed in further detail later on in the book); since the authors did not
# set the seed in the book, we cannot exactly reproduce their results, but by
# trying out different seed values, we can get the same results at least when
# rounded to a single digit

# plot growth versus vote and add the regression line (Figure 7.2b)
plot(vote ~ growth, data=dat, xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", pch=19, xaxt="n", yaxt="n",
     xlim=c(-0.5,4.5), ylim=c(43,62), bty="l")
abline(h=50, col="gray")
points(vote ~ growth, data=dat, pch=19)
axis(side=1, at=0:4, labels=paste0(0:4, "%"))
axis(side=2, at=c(45,50,55,60), labels=paste0(c(45,50,55,60), "%"))
title("Data and linear fit")
abline(res, lwd=3)
text(3, coef(res)[1] + coef(res)[2]*3, pos=4, offset=2,
     paste0("y = ", formatC(coef(res)[1], digits=1, format="f"),
            " + ", formatC(coef(res)[2], digits=1, format="f"), " x"))

# estimated slope plus/minus one standard error and two standard errors
round(coef(res)[2] + c(-1,1) * 1 * se(res)[2], digits=1)
round(coef(res)[2] + c(-1,1) * 2 * se(res)[2], digits=1)

# highlight the point for the 2008 election
points(vote ~ growth, data=dat, subset=year==2008, pch=19, cex=2, col="red")
text(dat$growth[dat$year==2008], dat$vote[dat$year==2008], "2008", pos=1, cex=0.8)

## Graphing the fitted regression line

# note: we can just do abline(res) as shown above, so we do not have to use
# abline(coef(res)) as described in the book

## Using the model to predict

# based on our results we obtained above, the predicted vote share for Clinton
# in 2016 was as follows (based on growth =~ 2% for the second term of Obama)
46.3 + 2*3.0

# Figure 7.3
xs <- seq(35, 70, length=1000)
ys <- dnorm(xs, mean=52.3, sd=3.9)
plot(xs, ys, type="l", bty="n", lwd=3, yaxs="i", yaxt="n", ylim=c(0,max(ys)+.005),
     xlab="Clinton share of the two-party vote", ylab="")
xs.sub <- seq(50, 70, length=1000)
ys.sub <- dnorm(xs.sub, mean=52.3, sd=3.9)
polygon(c(xs.sub,rev(xs.sub)), c(ys.sub,rep(0,length(xs.sub))), col="gray")
text(50, dnorm(50, mean=52.3, sd=3.9)/3, "Predicted\n72% chance\nof Clinton victory", pos=4)
lines(xs, ys, lwd=3)

# proportion of the area above 50 in a normal distribution with mean 52.3 and
# standard deviation 3.9
pnorm(50, mean=52.3, sd=3.9, lower.tail=FALSE)

############################################################################

### 7.2: Checking the model-fitting procedure using fake-data simulation

## Step 1: Creating the pretend world
a <- 46.3
b <- 3.0
sigma <- 3.9
x <- dat$growth
n <- length(x)

## Step 2: Simulating fake data
set.seed(1234)
y <- a + b*x + rnorm(n, mean=0, sd=sigma)
fake <- data.frame(x, y)

## Step 3: Fitting the model and comparing fitted to assumed values
res <- stan_glm(y ~ x, data=fake)
res

# extract the coefficients and standard errors
b_hat <- coef(res)["x"]
b_se  <- se(res)["x"]

# check whether the 68% and 95% CIs include the true slope
cover_68 <- (b_hat - 1*b_se) < b && (b_hat + 1*b_se > b)
cover_95 <- (b_hat - 2*b_se) < b && (b_hat + 2*b_se > b)
cover_68
cover_95

## Step 4: Embedding the simulation in a loop

set.seed(1234)
n_fake <- 1000
cover_68 <- rep(NA, n_fake)
cover_95 <- rep(NA, n_fake)

pbar <- txtProgressBar(min=0, max=n_fake, style=3)

# note: the loop below can take around ~10 minutes to finish

for (s in 1:n_fake) {

   setTxtProgressBar(pbar, s)

   y <- a + b*x + rnorm(n, mean=0, sd=sigma)
   fake <- data.frame(x, y)
   res <- stan_glm(y ~ x, data=fake, refresh=0) # suppress output on console
   b_hat <- coef(res)["x"]
   b_se  <- se(res)["x"]
   cover_68[s] <- (b_hat - 1*b_se) < b && (b_hat + 1*b_se > b)
   cover_95[s] <- (b_hat - 2*b_se) < b && (b_hat + 2*b_se > b)

}

# check the coverage of the 68% and 95% CIs
mean(cover_68)
mean(cover_95)

# rerun the simulation using critical t-values

cover_68 <- rep(NA, n_fake)
cover_95 <- rep(NA, n_fake)

# note: for a 68% CI, we need the value, say t_crit, from a t-distribution
# under which 84% of the area falls, since that leaves 16% above and since we
# construct a two-sided CI, a total of 32% will fall outside the interval
# (-t_crit, t_crit); similarly, for the 95% CI, we need the value under which
# 97.5% of the area falls
t_68 <- qt(0.84,  df=n-2)
t_95 <- qt(0.975, df=n-2)

for (s in 1:n_fake) {

   setTxtProgressBar(pbar, s)

   y <- a + b*x + rnorm(n, mean=0, sd=sigma)
   fake <- data.frame(x, y)
   res <- stan_glm(y ~ x, data=fake, refresh=0) # suppress output on console
   b_hat <- coef(res)["x"]
   b_se  <- se(res)["x"]
   cover_68[s] <- (b_hat - t_68*b_se) < b && (b_hat + t_68*b_se > b)
   cover_95[s] <- (b_hat - t_95*b_se) < b && (b_hat + t_95*b_se > b)

}

# check the coverage of the 68% and 95% CIs
mean(cover_68)
mean(cover_95)

############################################################################

### 7.3: Formulating comparisons as regression models