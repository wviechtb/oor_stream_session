############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-08-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.6 - ?
#
# last updated: 2023-08-17

############################################################################

### 3.6: Probability modeling

## Using an empirical forecast

# switch off scientific notation for this example
options(scipen=100)

# simulate y (proportion of the votes received by one of the candidates) 10^7
# times and then compute the probability of seeing exactly 1000 votes for each
# candidate in 2000 voters (this gives an empirical estimate of seeing an
# equal split of the votes using a simulation approach)
n <- 2000
y <- rnorm(10000000, mean=0.49, sd=0.04)
mean(round(y * n) == n/2)

# use the equation in the book to obtain the same value
dnorm(0.5, mean=0.49, sd=0.04) / n

# we can confirm that when we change n (to say 20,000 or 200,000), this still
# works, but when n is very large, we need to increase how many values of y we
# simulate to get an accurate estimate with the simulation approach

# actually, to do this absolutely correctly, what we need to figure out is the
# probability of seeing a value of y that would give us an even split of the
# votes when we multiply y with n (and round); this will happen when the value
# of y is between 0.5 - 1/(2*n) and 0.5 + 1/(2*n); that is, when y is between:
0.5 - 1/(2*n)
0.5 + 1/(2*n)

# since then y multiplied by n (and rounded) gives us an even split
round((0.5 - 1/(2*n)) * n)
round((0.5 + 1/(2*n)) * n)

# we can compute the probability of seeing such a y with pnorm() as follows
pnorm(0.5+1/(2*n), mean=0.49, sd=0.04) - pnorm(0.5-1/(2*n), mean=0.49, sd=0.04)

# the larger n is, the closer the equation from the book is to the correct
# value (and for 200,000, there is essentially no difference)
n <- 200000
dnorm(0.5, mean=0.49, sd=0.04) / n
pnorm(0.5+1/(2*n), mean=0.49, sd=0.04) - pnorm(0.5-1/(2*n), mean=0.49, sd=0.04)

# set scipen back to the default
options(scipen=0)
