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

# write out the possible paths for each conjecture
conj1 <- expand.grid(replicate(3, c("W","W","W","W"), simplify=FALSE))
conj2 <- expand.grid(replicate(3, c("B","W","W","W"), simplify=FALSE))
conj3 <- expand.grid(replicate(3, c("B","B","W","W"), simplify=FALSE))
conj4 <- expand.grid(replicate(3, c("B","B","W","W"), simplify=FALSE))
conj5 <- expand.grid(replicate(3, c("B","B","B","W"), simplify=FALSE))
conj6 <- expand.grid(replicate(3, c("B","B","B","B"), simplify=FALSE))

# examine the one for the second conjecture
conj2

# compute the number of paths in each conjecture that match the data
sum(rowSums(obs == conj1) == 3)
sum(rowSums(obs == conj2) == 3)
sum(rowSums(obs == conj3) == 3)
sum(rowSums(obs == conj4) == 3)
sum(rowSums(obs == conj5) == 3)

# now we draw another marble and it is blue
obs <- c("B","W","B","B")

# compute the number of paths in each conjecture that match the data
sum(rowSums(obs == conj1) == 3)
sum(rowSums(obs == conj2) == 3)
sum(rowSums(obs == conj3) == 3)
sum(rowSums(obs == conj4) == 3)
sum(rowSums(obs == conj5) == 3)
