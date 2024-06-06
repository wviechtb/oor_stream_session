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

d / sum(d)

# note that these are exactly identical to the plausibility values we computed
# earlier by going through the garden of forking data

### 2.3: Components of the model


############################################################################
