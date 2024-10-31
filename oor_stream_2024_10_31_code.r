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

# back to the dataset; select from dat only those who are 18 years or older
dat <- dat[dat$age >= 18,]

# create the grid for the grid approximation
mu.list <- seq(from=150, to=160, length.out=100)
sigma.list <- seq(from=7, to=9, length.out=100)
post <- expand.grid(mu=mu.list , sigma=sigma.list)
head(post, 10)

# select the five people from the full dataset
sub <- dat[41:45,]

# compute the likelihood of the data for every combination of mu and sigma in
# the grid; that is, we compute the density of the observed height values
# under a normal distribution for given values of mu and sigma and then
# compute the product to get the joint density; however, since here we
# consider mu and sigma as unknown, we call the resulting value a 'likelihood'
post$likelihood <- sapply(1:nrow(post), function(i) prod(dnorm(sub$height, mean=post$mu[i], sd=post$sigma[i])))

# then we multiple the likelihood values by the prior plausibilities for mu
# and by the prior plausibilities of sigma (where we use a normal distribution
# to reflect our prior knowledge about mu and a uniform distribution to
# reflect out prior knowledge about sigma)
post$prob <- post$likelihood * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)
head(post)

# this then yields the posterior plausibility of a certain combination of mu
# and sigma in our grid (i.e., in essence, except for scaling, the posterior
# joint distribution of mu and sigma)

# determine which combination of mu and sigma in the grid is most plausible
which.max(post$prob)
post[which.max(post$prob),]

# note that what we are doing above is the same as what we did for the globe
# tossing example in section 2.4.3

# draw the 3-dimensional surface for the posterior plausibilities for each
# combination of mu and sigma in the grid
tmp <- split(post, post$sigma)
tmp <- sapply(tmp, function(x) x$prob)
rownames(tmp) <- mu.list
colnames(tmp) <- sigma.list
tmp[1:5,1:5]
persp(mu.list, sigma.list, tmp, theta=45, phi=25, shade=0.2, ticktype="detailed",
      xlab="mu", ylab="sigma", zlab="posterior plausibility")

# instead of drawing this 3d surface, we can use a filled contour plot
filled.contour(mu.list, sigma.list, tmp, color.palette=hcl.colors, xlab="mu", ylab="sigma")

# why did I do all of the above based on only 5 people? because when we
# compute the product for the likelihood, then we run into numerical problems
# because the values we are multiplying are close to zero and the resulting
# value cannot be distinguished from 0 (note: below we now use dat$height)
post$likelihood <- sapply(1:nrow(post), function(i) prod(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i])))
post$prod <- post$likelihood * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)
head(post)

# to get around this problem, we will compute the log likelihood values with log(prod(x)) = sum(log(x))
post$likelihood <- NULL
post$prod <- NULL
post$ll <- sapply(1:nrow(post), function(i) sum(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i], log=TRUE)))
head(post)

# and now instead of computing likelihood * prior-mu * prior-sigma, we compute
# log(likelihood * prior-mu * prior-sigma) = log(likelihood) + log(prior-mu) + log(prior-sigma)
post$prod <- post$ll + dnorm(post$mu, mean=178, sd=20, log=TRUE) + dunif(post$sigma, min=0, max=50, log=TRUE)
head(post)

# in the last step, we exponentiate the values to get the posterior
# plausibilities (before doing so, we subtract the maximum value so that the
# values we are exponentiating are not quite so negative, again to avoid
# numerical issues)
post$prob <- exp(post$prod - max(post$prod))
head(post)

# determine which combination of mu and sigma in the grid is most plausible
which.max(post$prob)
post[which.max(post$prob),]

# draw the 3-dimensional surface for the posterior plausibilities
tmp <- split(post, post$sigma)
tmp <- sapply(tmp, function(x) x$prob)
rownames(tmp) <- mu.list
colnames(tmp) <- sigma.list
persp(mu.list, sigma.list, tmp, theta=25, phi=25, shade=0.2, ticktype="detailed",
      xlab="mu", ylab="sigma", zlab="posterior plausibility")

# instead of drawing this 3d surface, we can use a filled contour plot
filled.contour(mu.list, sigma.list, tmp, color.palette=hcl.colors, xlab="mu", ylab="sigma")

# using the contour_xyz() and image_xyz() functions from the rethinking
# package, we can avoid having to do the restructuring of the post data frame
# as we did above for drawing the (filled) contour plot
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob, col=hcl.colors(100))

## 4.3.4: Sampling from the posterior

# sample 10,000 values from the grid in accordance with the posterior
# plausibilities of the various combinations of mu and sigma
sample.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

############################################################################
