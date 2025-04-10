############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-10
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 6.1 - ?
#
# last updated: 2025-04-10

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

# Overthinking: Simulated science distortion

set.seed(1914)
N <- 200 # number of grant proposals
p <- 0.1 # proportion to select
nw <- rnorm(N) # simulate newsworthiness scores
tw <- rnorm(N) # simulate trustworthiness scores
s <- nw + tw   # compute the total score s
q <- quantile(s, 1-p) # find the top 10% threshold
selected <- s >= q # select a proposal if it has a top 10% combined score
# plot the scores against each other with blue points for the selected proposals
plot(tw, nw, pch=21, bg=ifelse(selected, "dodgerblue", "white"), bty="l",
     xlab="trustworthiness", ylab="newsworthiness")
cor(tw[selected], nw[selected]) # negative correlation among the selected proposals

############################################################################

### 6.1: Multicollinearity

## 6.1.1: Multicollinear legs

set.seed(909)
N <- 100 # number of individuals
height   <- rnorm(N, mean=10, sd=2) # simulate total height of each individual
leg_prop <- runif(N, 0.4, 0.5)      # leg as proportion of height
leg_left  <- leg_prop*height + rnorm(N, 0, 0.02) # sim left leg as proportion + error
leg_right <- leg_prop*height + rnorm(N, 0, 0.02) # sim right leg as proportion + error
dat <- data.frame(height, leg_left, leg_right) # combine into data frame
head(dat)

# where does the 2.2 in the book come from?
# if height (y) = 0,  then leg (x) = 0
# if height (y) = 10, then leg (x) = 10*0.45 = 4.5
# so this implies a slope of: (10 - 0) / (4.5 - 0) = 10 / 4.5 =~ 2.2

# fit the model predicting height from leg_left and leg_right
res1 <- quap(alist(height ~ dnorm(mu, sigma),
                   mu <- a + bl*leg_left + br*leg_right,
                   a ~ dnorm(10, 100),
                   bl ~ dnorm(2, 10),
                   br ~ dnorm(2, 10),
                   sigma ~ dexp(1)), data=d)
precis(res1, prob=0.95)

# plot the posterior means and corresponding 95% intervals
op <- par(no.readonly=TRUE)
plot(precis(res1, prob=0.95))
par(op)

# extract samples from the posterior distributions
post <- extract.samples(res1)
head(post)

# Figure 6.2 (left): plot of the sampled values for the regression coefficients
plot(bl ~ br, data=post, col=adjustcolor("#1e59ae",alpha.f=0.1), pch=16)

# compute the sum of the sampled values
sum_blbr <- post$bl + post$br

# Figure 6.2 (right): plot of the kernel density estimate of the sum
plot(density(sum_blbr), lwd=5, col="#1e59ae", xlab="sum of bl and br", main="", bty="l")

# fit the model predicting height from only leg_left
res2 <- quap(alist(height ~ dnorm(mu, sigma),
                   mu <- a + bl*leg_left,
                   a ~ dnorm(10, 100),
                   bl ~ dnorm(2, 10),
                   sigma ~ dexp(1)), data=d)
precis(res2, prob=0.95)

## 6.1.2: Multicollinear milk