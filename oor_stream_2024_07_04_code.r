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

## 2.4.1. Bayes’ theorem.

# as we saw last time, the likelihood function for seeing 6 times water out of
# 9 globe tosses can be computed based on a binomial distribution where the
# data are fixed and we change the true probability (p) to various values
# between 0 and 1

ps <- seq(0, 1, length=1000)
ls <- dbinom(6, size=9, prob=ps)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Likelihood")

# if we assume each value of p is equally plausible to begin with, then the
# prior distribution for p is flat

prior1 <- rep(1, 1000)
prior1 <- prior1 / sum(prior1)
plot(ps, prior1, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Prior Probability")

# then we get the following posterior distribution for p

post1 <- ls * prior1
post1 <- post1 / sum(post1)
plot(ps, post1, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# however, we could also think that values of p between 0 and 0.5 are
# completely impossible, but every value between 0.5 and 1 is equally
# plausible

prior2 <- c(rep(0,500), rep(1,500))
prior2 <- prior2 / sum(prior2)
plot(ps, prior2, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Prior Probability")

# then the posterior will look as follows

post2 <- ls * prior2
post2 <- post2 / sum(post2)
plot(ps, post2, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# finally, we might think that values of p around 0.5 are more plausible than
# values close to 0 or 1; we could express this in terms of the following
# equation

prior3 <- 1/(0.2*2) * exp(-abs(ps-0.5)/0.2)
prior3 <- prior3 / sum(prior3)
plot(ps, prior3, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Prior Probability")

# then the posterior will look as follows

post3 <- ls * prior3
post3 <- post3 / sum(post3)
plot(ps, post3, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# let's put all of this into a single figure (Figure 2.6)

par(mfrow=c(3,3))
plot(ps, prior1, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
mtext("prior", side=3, line=2, font=2)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
mtext("likelihood", side=3, line=2, font=2)
plot(ps, post1, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
mtext("posterior", side=3, line=2, font=2)
plot(ps, prior2, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, ls, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, post2, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, prior3, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, ls, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
plot(ps, post3, type="l", lwd=3, bty="l", xlab="", ylab="", yaxt="n")
