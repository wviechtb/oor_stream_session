############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-05
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 3.1 - ?
#
# last updated: 2024-09-05

############################################################################

# code for the vampire test example

Pr_Positive_Vampire <- 0.95 # also called the sensitivity of the test
Pr_Positive_Mortal  <- 0.01 # 1-this is called the specificity of the test (0.99)
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
               Pr_Positive_Mortal  * (1 - Pr_Vampire)
Pr_Vampire_Positive <- Pr_Positive_Vampire * Pr_Vampire / Pr_Positive
Pr_Vampire_Positive

#                  vampire      mortal
#               +-----------+-----------+
# test positive | (3)    95 | (5)   999 |   1094 (7)
#               +-----------+-----------+
# test negative | (4)     5 | (6) 98901 |  98906
#               +-----------+-----------+-----------
#                 (2)   100       99900 | 100000 (1)
#
# (1) Let's assume we are looking at a population of 100,000 individuals.
# (2) Given Pr_Vampire, we can then compute how many of them are vampires and
#     how many are mortals.
# (3) Given Pr_Positive_Vampire, we can then compute how many of the vampires
#     will test positive.
# (4) We can then also fill in this number.
# (5) Given Pr_Positive_Mortal, we can then compute how many of the mortals
#     will test positive.
# (6) We can then also fill in this number.
# (7) We can now also fill in the row totals.
# (8) Now that we have completed the table, we can easily compute the
#     probability that a person is a vampire given that the test is positive:
#     namely, 95 / 1094

95 / 1094

# one could then take the additional step of setting N in (1) to 1, since then
# all of the numbers in the calculations above can be thought of as
# probabilities and we can then see how the equation above arises

############################################################################

### 3.1: Sampling from a grid-approximate posterior

p_grid <- seq(from=0, to=1, length.out=1000)
prob_p <- rep(1, 1000) # prior
prob_data <- dbinom(6, size=9, prob=p_grid) # likelihood
plot(p_grid, prob_data, type="l") # plot the likelihood function
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type="l") # plot the posterior distribution

# sample values of p in accordance with how probable the values are (which we
# have determined above using our grid approximation)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

############################################################################
