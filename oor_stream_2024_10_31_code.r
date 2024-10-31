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

# select the first five people from the full dataset
dat <- dat[1:5,]

# compute the likelihood of the data for every combination of mu and sigma in
# the grid; that is, we compute the density of the observed height values
# under a normal distribution for given values of mu and sigma and then
# compute the product to get the joint density; however, since here we
# consider mu and sigma as unknown, we call the resulting value a 'likelihood'
post$likelihood <- sapply(1:nrow(post), function(i) prod(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i])))

# then we multiple the likelihood values by the prior plausibilities for mu
# and by the prior plausibilities of sigma (where we use a normal distribution
# to reflect our prior knowledge about mu and a uniform distribution to
# reflect out prior knowledge about sigma)
post$prod <- post$likelihood * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)
head(post)

# this then yields the posterior plausibility of a certain combination of mu
# and sigma in our grid (i.e., in essence, except for scaling, the posterior
# joint distribution of mu and sigma)

# determine which combination of mu and sigma in the grid is most plausible
which.max(post$prod)
post[which.max(post$prod),]


# compute the log likelihood of the data for every combination of mu and sigma in the grid
post$ll <- sapply(1:nrow(post), function(i) sum(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i], log=TRUE)))

exp(post$ll) * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)

post$prod <- post$ll + dnorm(post$mu, 178, 20, log=TRUE) + dunif(post$sigma, 0, 50, log=TRUE)


post$prob <- exp(post$prod - max(post$prod))


############################################################################
