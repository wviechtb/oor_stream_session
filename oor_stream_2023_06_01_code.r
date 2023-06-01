############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-06-01
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.1 - ?
#
# last updated: 2023-06-01

############################################################################

### 3.1: Weighted averages

# weighted average of the ages of people in the US, Mexico, and Canada
(310000000 * 36.8 + 112000000 * 26.7 + 34000000 * 40.7) / (310000000 + 112000000 + 34000000)

# create vectors with the mean age values and population sizes
mean.age <- c(36.8, 26.7, 40.7)
pop.size <- c(310000000, 112000000, 34000000)

# use weighted.mean() to obtain the weighted average
weighted.mean(mean.age, pop.size)

# the weights that are used are for computing the weighted average
pop.size / sum(pop.size)

# another example: prevalence of hypertension in the US, Mexico, and Canada
# (based on some very quick googling)
prevalence <- c(0.47, 0.18, 0.25)
weighted.mean(prevalence, pop.size)

# so among all 456 million people in North America, about 38% have hypertension

### 3.2: Vectors and matrices

# download the dataset for this example from the book website
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data
dat <- read.table("hibbs.dat", header=TRUE)

# inspect the dataset
dat

# fit a regression model predicting the vote variable from the growth variable
res <- lm(vote ~ growth, data=dat)
summary(res)

# show only the regression coefficients rounded to one decimal place
round(coef(res), 1)

# note: in the book, the coefficients shown are based on the Bayesian model
# that was fitted in chapter 1; here, let's stick to the non-Bayesian results

# so, the model says: predicted vote = 46.2 + 3.1 * growth

# predicted vote when growth is equal to -1
predict(res, newdata=data.frame(growth=-1))

# predicted vote when growth is equal to 0
predict(res, newdata=data.frame(growth=0))

# predicted vote when growth is equal to 3
predict(res, newdata=data.frame(growth=3))

# we can do this in a single line of code and include even more growth values
newdat <- data.frame(growth=-1:4)
cbind(newdat, pred=predict(res, newdata=newdat))
