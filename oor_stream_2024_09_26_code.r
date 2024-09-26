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

