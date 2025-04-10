############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-10
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 6.1 - ?
#
# last updated: 2025-04-10

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

# Overthinking: Simulated science distortion

set.seed(1914)
N <- 200 # number of grant proposals
p <- 0.1 # proportion to select
nw <- rnorm(N) # simulate newsworthiness scores
tw <- rnorm(N) # simulate trustworthiness scores
s <- nw + tw   # compute the total score s
q <- quantile(s, 1-p) # find the top 10% threshold

# select top 10% of combined scores

selected <- ifelse( s >= q , TRUE , FALSE )
cor( tw[selected] , nw[selected] )

############################################################################
