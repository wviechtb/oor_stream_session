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

# download the dataset (need to do this once)
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# load the rstanarm package
library(rstanarm)

# fit a linear regression model predicting the kids' score from the moms' IQ
res <- stan_glm(kid_score ~ mom_iq, data=dat, refresh=0)
res

plot(kidiq$mom_iq, kidiq$kid_score, xlab="Mother IQ score", ylab="Child test score") abline(coef(fit_2)[1], coef(fit_2)[2])