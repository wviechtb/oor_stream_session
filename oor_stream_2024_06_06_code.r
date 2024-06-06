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

obs <- c("B","W","B")
sapply(conjs, function(x) count(x, obs))

obs <- c("B","W","B","B")
sapply(conjs, function(x) count(x, obs))




# write out the possible paths for each conjecture
conj1 <- expand.grid(replicate(3, c("W","W","W","W"), simplify=FALSE))
conj2 <- expand.grid(replicate(3, c("B","W","W","W"), simplify=FALSE))
conj3 <- expand.grid(replicate(3, c("B","B","W","W"), simplify=FALSE))
conj4 <- expand.grid(replicate(3, c("B","B","B","W"), simplify=FALSE))
conj5 <- expand.grid(replicate(3, c("B","B","B","B"), simplify=FALSE))

# examine the one for the second conjecture
conj2

# compute the number of paths in each conjecture that match the data
sum(colSums(obs == t(conj1)) == 3)
sum(colSums(obs == t(conj2)) == 3)
sum(colSums(obs == t(conj3)) == 3)
sum(colSums(obs == t(conj4)) == 3)
sum(colSums(obs == t(conj5)) == 3)

# now we draw another marble and it is blue
obs <- c("B","W","B","B")

# write out the possible paths for each conjecture
conj1 <- expand.grid(replicate(4, c("W","W","W","W"), simplify=FALSE))
conj2 <- expand.grid(replicate(4, c("B","W","W","W"), simplify=FALSE))
conj3 <- expand.grid(replicate(4, c("B","B","W","W"), simplify=FALSE))
conj4 <- expand.grid(replicate(4, c("B","B","B","W"), simplify=FALSE))
conj5 <- expand.grid(replicate(4, c("B","B","B","B"), simplify=FALSE))

# compute the number of paths in each conjecture that match the data
sum(colSums(obs == t(conj1)) == 4)
sum(colSums(obs == t(conj2)) == 4)
sum(colSums(obs == t(conj3)) == 4)
sum(colSums(obs == t(conj4)) == 4)
sum(colSums(obs == t(conj5)) == 4)

############################################################################

