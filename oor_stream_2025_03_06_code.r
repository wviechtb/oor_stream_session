############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-06
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.5 - ?
#
# last updated: 2025-03-06

############################################################################

# load the rstanarm package
library(rstanarm)

### 12.5: Other transformations

## Square root transformations

# download the dataset if it doesn't already exist
if (!file.exists("earnings.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the first six rows of the dataset
head(dat)

# fit a model predicting sqrt(earnings) from height
res <- stan_glm(sqrt(earn) ~ height, data=dat, refresh=0)
res

# plot of earnings versus height
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", ylab="earnings", bty="l", ylim=c(0,200000))

# extract the sampled values for the posterior distributions
post <- as.data.frame(res)
head(post)

# add 10 regression lines based on these posterior samples
xs <- seq(min(dat$height), max(dat$height), length.out=1000)
apply(post[1:10,], 1, function(b) lines(xs, (b[1] + b[2]*xs)^2))

## Using discrete rather than continuous predictors

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# model predicting the kids' test score from mom_work treated as a categorical variable
res <- stan_glm(kid_score ~ factor(mom_work), data=dat, refresh=0)
res

## Index and indicator variables

# download the dataset if it doesn't already exist
if (!file.exists("naes04.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Gay/data/naes04.csv", destfile="naes04.csv")

# read in the dataset
dat <- read.csv("naes04.csv")

# inspect the first six rows of the dataset
head(dat)

# turn everybody age 91+ into 91 year olds
dat$age[dat$age >= 91] <- 91

# create a frequency table of support versus age
tab <- table(dat$age, dat$gayFavorStateMarriage)
head(tab)

# turn the frequencies into percentages
tab <- prop.table(tab, margin=1) * 100
head(tab)

# add age as a proper variable to the table
tab <- cbind(tab, age=as.numeric(rownames(tab)))
head(tab)

# turn tab into a data frame
tab <- data.frame(tab)

# Figure 12.7 (left): plot of gayFavorStateMarriage versus age
plot(Yes ~ age, data=tab, pch=21, bg="gray", bty="l", ylim=c(0,60), las=1,
     xlab="Age", ylab="Support for same-sex marriage (%)")

# fit the model (using the aggregated data)
res1 <- stan_glm(Yes ~ age, data=tab, refresh=0)
res1

# add the regression line to the plot
abline(res1, lwd=3)

# compute the (median) R^2
median(bayes_R2(res1))

# discretize the age variable into 7 groups
breaks <- c(0, seq(29, 79, 10), 100)
tab$age_discrete <- cut(tab$age, breaks = breaks)
head(tab, 20)

# fit the model (using the aggregated data)
res2 <- stan_glm(Yes ~ age_discrete, data=tab, refresh=0)
res2

# Figure 12.7 (right): plot of gayFavorStateMarriage versus age
plot(Yes ~ age, data=tab, pch=21, bg="gray", bty="l", ylim=c(0,60), las=1,
     xlab="Age", ylab="Support for same-sex marriage (%)")

# add the regression line segments based on the model to the plot
for (i in 2:length(breaks)) {
   pred <- coef(res2)[1] + ifelse(i >= 3, coef(res2)[i-1], 0)
   segments(breaks[i-1], pred, breaks[i], pred, lwd=3)
}

## Indicator variables, identifiability, and the baseline condition

# fit the same model but exclude the intercept / constant term
res3 <- stan_glm(Yes ~ 0 + age_discrete, data=tab, refresh=0)
res3

### 12.6: Building and comparing regression models for prediction

# download the dataset if it doesn't already exist
if (!file.exists("mesquite.dat")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Mesquite/data/mesquite.dat", destfile="mesquite.dat")

# read in the data and inspect the first 6 rows
dat <- read.table("mesquite.dat", header=TRUE)
head(dat)

# fit the model where we use all variables as predictors of weight
res <- stan_glm(weight ~ diam1 + diam2 + canopy_height + total_height + density + group, data=dat)



