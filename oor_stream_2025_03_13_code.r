############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-13
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.2 - ?
#
# last updated: 2025-03-13

############################################################################

### 5.2: Masked relationship

# load the rethinking package
library(rethinking)

# get the milk dataset and put it into 'dat'
dat <- get(data(milk))

# inspect the first 6 rows
head(dat)

# standardize the three variables of interest
dat$K <- c(scale(dat$kcal.per.g))
dat$N <- c(scale(dat$neocortex.perc))
dat$M <- c(scale(log(dat$mass)))

# model predicting K from N
model <- alist(K ~ dnorm(mu, sigma),
               mu <- a + bN*N,
               a ~ dnorm(0, 1),
               bN ~ dnorm(0, 1),
               sigma ~ dexp(1))

# fit the model
res <- quap(model, data=dat)
