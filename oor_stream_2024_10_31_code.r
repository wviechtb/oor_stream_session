############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-10-31
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.3.3 - ?
#
# last updated: 2024-10-31

############################################################################

## 4.3.3: Grid approximation of the posterior distribution

# load the rethinking package
library(rethinking)

## 4.3.1: The data

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# examine the structure of the data frame
str(dat)

# get summary statistics and mini histograms for the variables
precis(dat)

# create the grid for the grid approximation
mu.list <- seq(from=150, to=160, length.out=100)
sigma.list <- seq(from=7, to=9, length.out=100)
post <- expand.grid(mu=mu.list , sigma=sigma.list)
head(post, 50)

# compute the log likelihood of the data for every combination of mu and sigma in the grid
post$ll <- sapply(1:nrow(post), function(i) sum(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i], log=TRUE)))

post$prod <- post$ll + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))


############################################################################
