############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-06-06
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 2.1 - ?
#
# last updated: 2024-06-06

############################################################################

### 2.1: The garden of forking data

# the observed marbles
obs <- c("B","W","B")

# a list with the five possible conjectures
conjs <- list(c("W","W","W","W"),
              c("B","W","W","W"),
              c("B","B","W","W"),
              c("B","B","B","W"),
              c("B","B","B","B"))

# function that takes a conjecture as input and the observed draws and returns
# the number of paths that one can take through the garden of forking data
# according to the conjecture given the observed data
count <- function(conj, obs) {
   n <- length(obs)
   conj <- expand.grid(replicate(n, conj, simplify=FALSE))
   sum(colSums(obs == t(conj)) == n)
}

# check that this returns the correct value (3) for conjecture 2
count(conjs[[2]], obs)

# now apply the function to all five conjectures
sapply(conjs, count, obs)

# now we draw another marble and it is blue
obs <- c("B","W","B","B")

# again compute the number of paths for each conjecture
sapply(conjs, count, obs)

# now make these counts the prior
prior <- sapply(conjs, count, obs)

# vector with the factory count data
factory <- c(0, 3, 2, 1, 0)

# the final count is the product of the two
prior * factory

# note: the count() function above creates a large data frame (conj) that can
# get very large when 'obs' is a long sequence, to the point that your
# computer can easily run out of memory

# we can do the same calculation as explained on page 23 by computing for each
# element in obs how many marbles match up in color for each element in a
# particular conjecture and then take the product term of these values; let's
# try this for the four observations above
sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))

# this will also work for longer observation sequences (although the counts
# themselves get very large) without things blowing up
obs <- sample(c("B","W"), 50, replace=TRUE)
sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))

# these counts get very large and their absolute values are not relevant;
# instead, we just care about the relative sizes
ways <- sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))
round(ways / sum(ways), 4)

# let's go back to the short sequence of three marbles
obs <- c("B","W","B")
ways <- sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))
data.frame(p = 0:4 / 4, ways = ways, plausibility = ways / sum(ways))

# as will be discussed in the next section, the kind of process that is
# described by the marble example is actually the process underlying the
# so-called binomial distribution; we can think of our observation as a count
# of the number of blue marbles in the three 'trials' where, on each draw,
# there is a given probability of seeing a blue marble according to a certain
# conjecture; for example, say 1 out of the 4 marbles in the bad are blue,
# then there is a 1/4 = 0.25 probability of drawing a blue marble on a single
# trial and hence a 1-0.25=0.75 probability of drawing a white marble; if we
# see the sequence above (blue, white, blue), then the corresponding
# probabilities are 0.25, 0.75, and 0.25 and since the draws are independent,
# the probability of seeing this specific sequence is just the product of
# these probabilities, namely:

0.25 * 0.75 * 0.25

# however, there are other sequences that lead to two blue marbles in the
# three trials, namely (white, blue, blue) and (blue, blue, white) and these
# sequences have the following probabilities:

0.75 * 0.25 * 0.25
0.25 * 0.25 * 0.75

# so there are 3 sequences leading to 2 blue marbles and so the probability of
# seeing any one of these three sequences is just the sum of their probabilities, namely:

0.25 * 0.75 * 0.25 + 0.75 * 0.25 * 0.25 + 0.25 * 0.25 * 0.75

# this is in fact what is computed by the equation on page 33, namely:

factorial(2 + 1) / (factorial(2)*factorial(1)) * 0.25^2 * (1-0.25)^1

# which is what dbinom() computes

dbinom(2, size=3, prob=1/4)

# now we can also already introduce the concept of 'likelihood' (which
# formally appears in section 2.3); the data we have (namely seeing 2 blue
# marbles in the 3 trials) are now given, but what we do not know if the value
# of p (the true probability of drawing a blue marble on a single trial);
# let's compute the probability of seeing the data we have under the 5
# possible conjectures

like <- c(dbinom(2, size=3, prob=0/4),
          dbinom(2, size=3, prob=1/4),
          dbinom(2, size=3, prob=2/4),
          dbinom(2, size=3, prob=3/4),
          dbinom(2, size=3, prob=4/4))
like

# when the data are 'fixed' and we compute these probabilities under different
# values of the unknown parameter p, then these values are called
# 'likelihoods' since they denote how likely the observed data are under the 5
# different conjectures; we do not typically care about the absolute values,
# but only the relative likelihoods, which we can compute by dividing each
# likelihood value by their sum

like / sum(like)

# note that these are exactly identical to the plausibilities we computed
# earlier by going through the garden of forking data

# now the one piece missing is the prior plausibilities of the different
# conjectures; we just multiply the likelihoods by these prior plausibilities
# to get the posterior plausibilities; for example, if we assume they are all
# equally likely a priori (i.e., 1/5th), then we get these values

like * c(1/5, 1/5, 1/5, 1/5, 1/5)

# on the other hand, if we know a priori the information that is given at the
# bottom of page 25, then we would get these values

like * c(0, 1, 2, 3, 0)

# note: the prior plausibilities do not have to add up to 1; so whether we
# compute these posterior values

like * c(0/6, 1/6, 2/6, 3/6, 0/6)

# or the ones above does not make a difference, since in the end, we just care
# about which posterior plausibility is largest

# however, if we want to think of the posterior plausibilities as
# probabilities, then we need to rescale them in the end so they sum up to 1

post <- like * c(0/6, 1/6, 2/6, 3/6, 0/6)
post / sum(post)

### 2.2: Building a model

# we will do what is shown in this section in the following part

### 2.3: Components of the model

# now let's do these computations in the context of the globe throwing
# example, where we see 6 W's in 9 trials; if the true probability of seeing a
# W on a single trial is 0.5, then the probability of seeing these data is

dbinom(6, size=9, prob=0.5)

# now again, since the true probability is unknown, we can compute this
# probability for every possible value between 0 and 1 and then create a plot
# of this likelihood function

ps <- seq(0, 1, length=1000)
ls <- dbinom(6, size=9, prob=ps)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Likelihood")

# find the value of p that is most likely given the data

ps[which.max(ls)]

# sidenote: this is the maximum likelihood estimator, which, not entirely
# surprisingly is equal to number of W's divided by the number of trials

6 / 9

# now we introduce the prior for p; we just need to multiply the likelihood
# values with the prior values, and *boom*, we have the posterior values

# for example, we could assume that every value of p is equally plausible to
# begin with; then the posterior values are essentially just the likelihood
# values, just rescaled

post <- ls * 1/1000
post

# which we can plot again to see the posterior distributions

plot(ps, post, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Posterior")

# here, we have gone straight from the flat/uniform prior plus the data to the
# posterior as shown in the bottom right of Figure 2.5; but now let's recreate
# the entire figure, one data point at a time

obs <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")
prior <- rep(1/1000, 1000)

par(mfrow=c(3,3))

for (i in 1:9) {

   ls <- dbinom(obs[i] == "W", 1, prob=ps)
   post <- ls * prior
   post <- post / sum(post)

   plot(ps, post, type="l", lwd=3, bty="l", xlab="True Probability",
        ylab="Posterior", ylim=c(0,.003))
   lines(ps, prior, lty="dashed")
   text(0, .003, paste0("n = ", i))

   prior <- post

}



############################################################################
