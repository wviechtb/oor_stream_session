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
head(post, 10)

for (i in 1:500) {
   sub <- dat[i:(i+5),]
   if (abs(mean(sub$height) - 155) < 2 && abs(sd(sub$height) - 8) < 0.2)
      stop()
}


# select the five people from the full dataset
sub <- dat[59:64,]

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
post$prod <- post$likelihood * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)
head(post)

# this then yields the posterior plausibility of a certain combination of mu
# and sigma in our grid (i.e., in essence, except for scaling, the posterior
# joint distribution of mu and sigma)

# determine which combination of mu and sigma in the grid is most plausible
which.max(post$prod)
post[which.max(post$prod),]

# note that what we are doing above is the same as what we did for the globe
# tossing example in section 2.4.3

# draw the 3-dimensional surface for the posterior plausibilities for each
# combination of mu and sigma in the grid
tmp <- split(post, post$sigma)
tmp <- sapply(tmp, function(x) x$prod)
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
post$ll <- sapply(1:nrow(post), function(i) sum(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i], log=TRUE)))

exp(post$ll) * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)

post$prod <- post$ll + dnorm(post$mu, 178, 20, log=TRUE) + dunif(post$sigma, 0, 50, log=TRUE)


post$prob <- exp(post$prod - max(post$prod))


############################################################################
