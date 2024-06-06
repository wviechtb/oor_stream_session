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

############################################################################
