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
# data are fixed and we change the true probability to various values between
# 0 and 1

ps <- seq(0, 1, length=1000)
ls <- dbinom(6, size=9, prob=ps)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Likelihood")



obs <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")
prior <- rep(1/1000, 1000)

par(mfrow=c(3,3))

for (i in 1:9) {

   ls <- dbinom(obs[i] == "W", 1, prob=ps)
   post <- ls * prior
   post <- post / sum(post)

   plot(ps, post, type="l", lwd=3, xlab="True Probability",
        ylab="Posterior Probability", ylim=c(0,.003))
   lines(ps, prior, lty="dashed")
   text(0, .0028, paste0("n = ", i), pos=4)

   prior <- post

}
