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

############################################################################

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

# create the X matrix
X <- as.matrix(cbind(intercept=1, newdat))
X

# create the column vector with the regression coefficients
b <- cbind(coef(res))
b

# multiply to get the predicted values manually
X %*% b

############################################################################

### 3.3: Graphing a line

# download the dataset corresponding to the example
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Mile/data/mile2.txt", destfile="mile2.txt")

# see Wikipedia for more details on this sporting event and the dataset
# https://en.wikipedia.org/wiki/Mile_run_world_record_progression

# read in the data
dat <- read.table("mile2.txt", header=TRUE)
dat

# create a variable that combines year and month (as a fraction of 12 months)
dat$year.month <- with(dat, yr + month / 12)

# create a variable that combines the min and sec variable into total seconds
dat$seconds <- with(dat, min*60 + sec)

# inspect the dataset
dat

# fit a regression model predicting seconds from year.month
res <- lm(seconds ~ year.month, data=dat)
summary(res)

# create a scatterplot of the data
plot(seconds ~ year.month, data=dat, xlim=c(1900,2001), ylim=c(215,265),
     xaxs="i", bty="l", pch=21, bg="gray", type="o", lty="dotted",
     xlab="Year", ylab="Time (seconds)")

# add the equation for the line
text(1950, 242, pos=4, expression(y == 1007 - 0.393*x))

# add the regression line from the model to the figure
abline(res, lwd=3)

# center year.month at 1900 to make the intercept more meaningful
res <- lm(seconds ~ I(year.month-1900), data=dat)
summary(res)

# now the intercept refers to the predicted time in 1900

############################################################################

### 3.4: Exponential and power-law growth and decline;
###      logarithmic and log-log relationships

# exponential growth example
curve(1.5*10^9 * 2^((x-1900)/50), from=1900, to=2000, lwd=3,
      xlab="Year", ylab="Population Size")

# draw the same line again, but now in the A*exp(b*x) form
curve(1.5*10^9 * exp(log(2)/50 * (x-1900)), from=1900, to=2000, lwd=3,
      xlab="Year", ylab="Population Size", add=TRUE, col="red")

# let's see what is really going in the world; let's get the data from Wikipedia
# https://en.wikipedia.org/wiki/World_population#Annual_population_growth

# unfortunately, this page only provides data starting at 1951; also, we will
# only use the data up to 1970 (since after that the growth is more or less
# linear, which kind of defeats the purpose of this little exercise)
dat <- structure(list(year = 1951:1970, size = c(2584034261, 2630861562,
2677608960, 2724846741, 2773019936, 2822443282, 2873306090, 2925686705,
2979576185, 3034949748, 3091843507, 3150420795, 3211001009, 3273978338,
3339583597, 3407922630, 3478769962, 3551599127, 3625680627, 3700437046)),
row.names = c(NA, 20L), class = "data.frame")

# examine the dataset
dat

# scatterplot of year versus size
plot(size ~ year, data=dat, pch=21, bg="gray", type="o", lty="dotted")

# if there is exponential growth, then the relationship between log(size) and
# year should be roughly linear and that is indeed the case
plot(log(size) ~ year, data=dat, pch=21, bg="gray", type="o", lty="dotted")

# fit the regression model (on the log scale)
res <- lm(log(size) ~ year, data=dat)
summary(res)

# the increase in popoulation size per 1-year increase
exp(1.882e-02)

# so we see that between 1951 and 1970, the world population increased by a
# factor of 1.02 (or roughly 2%) every year

# the increase in popoulation size per 10-year increase
exp(1.882e-02 * 10)

# for every 10 years, the population size increased by a factor of 1.21 (or 21%)

# to see why exp(b) gives the factor by which the population size increases
# for a one-year increase, note that the model is given by this:
#
# log(size) = a + b*year
#
# so to compute how much log(size) changes when year goes up by one unit, we
# can compute:
#
# log(size | year+1) - log(size | year) = (a + b*(year+1)) - (a + b*year)
#                                       = a + b*year + b - a - b*year
#                                       =              b
#
# since:
#
# log(size | year+1) - log(size | year) = log((size | year+1) / (size | year))
#
# then when we exponentiate both sides of the equation, we get:
#
# (size | year+1) / (size | year) = exp(b)

# data for Figure 3.4 (both variable in log units)
dat <- structure(list(bm = c(-3.86, -3.8, -1.22, -0.85, -0.83, -0.32, 1.04,
0.81, 1.01, 1.15, 1.08, 1.15, 1.52, 1.66, 2.41, 2.72, 2.99, 3.67, 3.68, 3.87,
4.13, 4.22, 4.91, 5.51, 6.2, 6.22, 6.42, 8.22 ), rate = c(-1.77, -1.22, 0.34,
0.48, 0.69, 0.86, 1.34, 1.8, 1.79, 1.87, 2.02, 2.18, 2.39, 2.64, 2.88, 3.17,
3.29, 3.68, 3.94, 4.06, 4.14, 4.41, 4.73, 5.6, 5.67, 6, 6.33, 7.65)),
row.names = c(NA, -28L), class = "data.frame")

plot(dat$bm, dat$rate, pch=19)

round(exp(dat$bm), 2)