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

#                 vampire   mortal
#               +---------+---------+
# test positive |         |         |
#               +---------+---------+
# test negative |         |         |
#               +---------+---------+------
#                                   | 10000



### 3.1: Sampling from a grid-approximate posterior



############################################################################
