############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-07-04
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 2.4 - ?
#
# last updated: 2024-07-04

############################################################################

### 2.4: Making the model go

# as we saw last time, the likelihood function for seeing 6 times water out of
# 9 globe tosses can be computed based on a binomial distribution where the
# data are fixed and we change the true probability (p) to various values
# between 0 and 1

ps <- seq(0, 1, length=1000)
ls <- dbinom(6, size=9, prob=ps)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Likelihood")

# if we assume each value of p is equally plausible to begin with, then the
# prior distribution for p is flat and we get the following posterior
# distribution for p

prior <- rep(1/1000, 1000)
post <- ls * prior
post <- post / sum(post)
plot(ps, post, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# however, we could also think that values of p between 0 and 0.5 are
# completely impossible, but every value between 0.5 and 1 is equally
# plausible; then the posterior will look as follows

prior <- c(rep(0,500), rep(1,500))
post <- ls * prior
post <- post / sum(post)
plot(ps, post, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# finally, we might think that values of p around 0.5 are more plausible than
# values close to 0 or 1; we

