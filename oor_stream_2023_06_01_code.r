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



1951  2584034261
1952  2630861562
1953  2677608960
1954  2724846741
1955  2773019936
1956  2822443282
1957  2873306090
1958  2925686705
1959  2979576185
1960  3034949748
1961  3091843507
1962  3150420795
1963  3211001009
1964  3273978338
1965  3339583597
1966  3407922630
1967  3478769962
1968  3551599127
1969  3625680627
1970  3700437046
1971  3775759617
1972  3851650245
1973  3927780238
1974  4003794172
1975  4079480606
1976  4154666864
1977  4229506060
1978  4304533501
1979  4380506100
1980  4458003514
1981  4536996762
1982  4617386542
1983  4699569304
1984  4784011621
1985  4870921740
1986  4960567912
1987  5052522147
1988  5145426008
1989  5237441558
1990  5327231061
1991  5414289444
1992  5498919809
1993  5581597546
1994  5663150427
1995  5744212979
1996  5824891951
1997  5905045788
1998  5984793942
1999  6064239055
2000  6143494000
2001  6222627000
2002  6301773000
2003  6381185000
2004  6461159000
2005  6541907000
2006  6623518000
2007  6705947000
2008  6789089000
2009  6872767000
2010  6956824000
2011  7041194000
2012  7125828000
2013  7210582000
2014  7295291000
2015  7379797000
2016  7464022000
2017  7547859000
2018  7631091000
2019  7713468000
2020  7795000000
